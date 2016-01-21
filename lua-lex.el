;; Facilities to lex lua code

(defmacro one-of (char str)
  "Returns if character exists in a string"
  `(assq ,char '(,@(loop for c across str collect (cons c t)))))

(defun find-next(start str)
  "Finds the next occurrence of a string in a buffer form a point"
  (loop for i from start to (point-max)
		if (loop for c across str
				 for j from i
				 if (not (eq c (char-after j))) do (return)
				 finally (return t)) do (return (+ (- i start) (length str)))))

(defun string-at (str pos)
  "Test if the pos is pointing at the given string"
  (loop for c across str
		for i from pos
		if (not (eq (char-after i) c)) do (return nil)
		finally (return t)))
		
(defmacro char-case (v &rest forms)
  "Case statement that dispatches gainst a set of characters
e.g
(char-chase c
  (\"abc\" 0)
  (\"def\" 1)))"
  (declare (indent defun))
  `(case ,v
	 ,@(mapcar (lambda (form) 
				 (let ((chars (car form))
					   (value (cdr form)))
				   (cons (cond ((eq 'otherwise chars) 'otherwise)
							   ((stringp chars) (map 'list #'identity chars))
							   ((symbolp chars) (map 'list #'identity (symbol-value chars))))
						 value))) forms)))

(defun match-number (start &optional decimal exp)
  "Finds a valid lexical lua token. Can be decimal, hex, double or expontent"
  (loop for i from start
		for c = (char-after i)

		until (or (one-of c "()+-%*/{}],:; \t\n=<>~[\"") (not c))
		if      (not (one-of c "0123456789.eE")) do (error "malformed number %c" c)
		else if (and decimal (eq ?. c)) do (error "malformed number, double decimal")
		else if (and exp (eq ?. c)) do (error "malformed number, exponent in decimal")
		else if (and exp (one-of c "eE")) do (error "malformed number, double exponent")

		if (eq ?. c) do (setq decimal t)
		else if (one-of c "eE") do (setq exp t)
		count t))

(defun match-hex-number (start)
  "Counts the length of a hexidecimal number token"
  (assert (eq ?0 (char-after start)))
  (assert (one-of (char-after (1+ start)) "xX"))
  (+ 2 (loop for i from (+ 2 start)
		for c = (char-after i)
		until (or (not c) (one-of c "()+-%*/{}],: \t\n=<>~[\""))
		if (not (one-of c "0123456789ABCDEFabcdef")) do (error "malformed number")
		count t)))

(defmacro next-is (char &optional offset)
  ""
  `(eq (char-after (+ (or ,offset 1) *token-start* )) ,char))

(defun next-is (char &optional offset)
  (setq offset (or offset 1))
  (eq (char-after (+ offset *token-start* )) char))

(defun set-token (type length)
  (setq *token-type* type)
  (setq *token-end* (+ *token-start* length)))

(defmacro set-token (type length)
  `(progn (setq *token-type* ,type)
		  (setq *token-end* (+ *token-start* ,length))))

(defun keyword-sym ()
  (let ((len (- *token-end* *token-start*)))
	(and (< len 9)  ; optimizations
		 (> len 1)
		 (one-of (char-after *token-start*) "abdefilnortuw")
		 (loop for keyword in '("and" "break" "do" "elseif" "else" "end" "false" 
								"for" "function" "if" "in" "local" "nil" "not" 
								"or" "repeat" "return" "then" "true" "until" 
								"while")
			   for symbol in '(and break do elseif else end false 
								   for function if in local lua-nil not 
								   or repeat return then true until 
								   while)
			   if (and (string-at keyword *token-start*)
					   (= (length keyword) len))
			   do (return symbol)))))

(defun next-lua-token (&optional current-token skip-white)
  "Finds the next lua token, including white space"
  (when (< (or current-token 1) (point-max))
	(setq *token-start* (or current-token 1))
	(char-case (char-after *token-start*)

	  (" \t\n" (set-token 'white (loop for j from *token-start*
									   while (one-of (char-after j) " \t\n") count t)))

	  ("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
	   (set-token 'id (loop for i from *token-start* to (1- (point-max))
							until (one-of(char-after i)" \t\n(){}[]+-%*/,:=<>~.\"';")
							count t))
	   (setq *token-type* (or (keyword-sym) 'id)))
	  ("(" (set-token 'lp 1))
	  (")" (set-token 'rp 1))
	  ("{" (set-token 'lc 1))
	  ("}" (set-token 'rc 1))
	  ("]" (set-token 'rb 1))
	  ("+" (set-token '+ 1))
	  ("%" (set-token '% 1))
	  ("*" (set-token '* 1))
	  ("/" (set-token '/ 1))
	  ("," (set-token 'comma 1))
	  ("#" (set-token 'pound 1))
	  (":" (set-token ': 1))
	  (";" (set-token 'semi 1))
	  ("-"           (char-case (char-after (1+ *token-start*))
					   ("0123456789." (set-token 'number (1+ (match-number (1+ *token-start*)))))
					   ("-"           (if (next-is ?\[ 2)
										  (set-token 'comment (find-next *token-start* "]]"))
										(set-token 'comment (find-next *token-start* "\n"))))
					   (otherwise      (set-token '- 1))))

	  ("=<>"
	   (if  (eq ?= (char-after (1+ *token-start*)))
		   (char-case (char-after *token-start*)
			 ("=" (set-token '== 2))
			 (">" (set-token '>= 2))
			 ("<" (set-token '<= 2)))
	   	 (char-case (char-after *token-start*)
					  ("<" (set-token '< 1))
					  (">" (set-token '> 1))
					  ("=" (set-token '= 1)))))
	  ("~" (set-token 'op (if (next-is ?=) 1 (error "illegal token ~"))))

	  ("." (char-case (char-after (1+ *token-start*))
			 ("." (if (eq ?. (char-after (+ 2 *token-start*))) (set-token 'vararg 3)
					(set-token 'op 2)))
			 ("0123456789" (set-token 'number (match-number *token-start*)))
			 (otherwise (set-token 'dot 1))))

	  ("[" (if (next-is ?\[) (set-token 'string (find-next *token-start* "]]"))
			 (set-token 'lb 1))) 

	  ("'\"" (set-token 'string 
						(loop with delim = (char-after *token-start*)
							  for i from (1+ *token-start*)
							  for escaping = nil then (and (eq c ?\\) (not escaping))
							  for c = (char-after i)
							  if (and (eq delim c) (not escaping)) do (return (- i *token-start* -1)))))
	  ("0" (set-token 'number 
					  (if (or (next-is ?x) (next-is ?X)) (match-hex-number *token-start*)
						(match-number *token-start*))))
	  ("123456789" (set-token 'number (match-number *token-start*))))))

(defun test (&optional current-token)
  (setq *token-start* (or current-token 1))
  (setq *token-end* (+ *token-start* (char-case (char-after *token-start*)
									   ("." (if (next-is ?.) 
												(if (next-is ?. 2) 2 3)
											  (match-number (1+ *token-start*))))
									   ("0123456789" (match-number *token-start*))
									   (otherwise 1)))))

(defmacro with-time (&rest body)
  (let ((v (gensym)))
	`(let ((*start* (current-time))
		   (,v (progn ,@body)))
	   (message (format-time-string "Time: %M:%S.%3N" (time-subtract (current-time) *start*)))
	   ,v)))

(defsubst next-real-lua-token ()
  "Finds the next token, omitting white space"
  (loop for token = (next-lua-token *token-end*)
		while (and token (member *token-type* '(white comment)))
		finally (return token)))

(defsubst token-string ()
  "Gets token value"
  (buffer-substring-no-properties *token-start* *token-end*))

(defun token-types (string)
  "List the token types in a string, omitting white space"
  (with-temp-buffer 
	(insert string)
	(loop with *token-start* = 1 
		  with *token-end*   = 1
		  with *token-type* = nil
		  with before = '("else" "elseif" "}" "end" "until")
		  for token = (next-real-lua-token)
		  while token
		  collect *token-type*)))

(defun lua-find-def ()
  "Finds the declaration of the Lua symbol which the cursor is on"
  (interactive)
  (let ((p (find-def)))
	(if p (goto-char p))))

(defun find-def ()
  "Finds the definition of the symbol under point"
  ;; helper functions
  ;; dynamic scope assumed, env and cenv are defined in loop macro
  (flet ((pop-env () (setq cenv (pop env)))
		 (push-env () (push cenv env) (setq cenv nil))
		 (set-state (sym) (setq state sym))
		 (push-token (&optional s e) 
					 (push  (cons (buffer-substring-no-properties (or s *token-start*)
																  (or e *token-end*))
								  (or s *token-start*)) cenv))
		 (handle-for-signature ()
							   (push-env)
							   (next-real-lua-token) ;; id
							   (push-token)
							   (next-real-lua-token)
							   (if (eq 'comma *token-type*) (loop for _ = (next-real-lua-token)
																  until (eq *token-type* 'in)
																  do (case *token-type*
																	   (comma)
																	   (id (push-token)))))
							   (loop for _ = (next-real-lua-token)
									 until (eq *token-type* 'do))
							   (set-state 'top))
		 (handle-function-signature (&optional local)
									(loop for token = (next-real-lua-token) 
										  while (not (eq *token-type* 'lp))
										  for s = *token-start* then s
										  for e = *token-end*
										  finally (progn (push-token s e)
														 (push-env)))
									(loop for token = (next-real-lua-token)
										  do (case *token-type*
											   (rp (set-state 'top) (return))
											   (id (push-token))
											   (vararg (push-token))
											   (comma)
											   (otherwise (error "bad token in args list"))))))
	(loop with point-of-interest =  (progn (message "%s" (point)) (point))
		  with state = 'top
		  with cenv = nil
		  with env = nil
		  with *token-start* = 1 
		  with *token-end*   = 1 
		  with *token-type* = nil
		  for token = (next-real-lua-token) while token
		  for _ = (message "%s" (list *token-start* *token-end* point-of-interest))
		  if (and (>= point-of-interest *token-start*)
				  (< point-of-interest *token-end*)) do (return (loop with s = (token-string)
																	   for e in (cons cenv env)
																	   for v = (assoc s e) 
																	   if v do (return (progn
																						 (message "%s"
																								  (list *token-start* *token-end* v))
																						 (cdr v)))))
				  else if (< point-of-interest *token-start*) do (return)
				  do (case state
					   (top (case *token-type*
							  (function (handle-function-signature))
							  (for (handle-for-signature))
							  (local (set-state 'local-def))
							  ((do while repeat then) (push-env))
							  ((end until elseif) (pop-env))
							  (else (pop-env) (push-env))
							  (otherwise (set-state 'top))))
					   (local-def (case *token-type*
									(function (handle-function-signature t))
									(id (progn (push-token)
											   (set-state 'local-def-2)))
									(otherwise (error "bad token after 'local'"))))
					   (local-def-2 (case *token-type*
									  (comma) ;continue
									  ((= semi if for local function do return while repeat) (set-state 'top))
									  (id (push-token))
									  (otherwise (error "bad token in local def %s" (token-string)))))))))
;; tests
(defun test-find-def ()
  (flet ((test (str p e)
			   (with-temp-buffer
				 (insert str)
				  (goto-char p)
				  (eq e (find-def)))))
	(and 
	 (test "local b = 10; return b" 22 7)
	 (test "function a (b) function c(e) return b end end" 37 13)
	 (test "function a (b) function c(b) return b end end" 37 27)
	 (test "function a (b) local      e; return e end end" 37 27)
	 (test "function a () end a()" 19 10)
	 (test "function a (b,c) f(b,c) end" 22 15)
	 (test "local function a () end a()" 25 16)
	 (test "local a = 0 for i=1,20 do a = i + a end" 31 17)
	 (test "local a = 0 for i=1,20 do a = i + a end" 35 7)
	 (test "local a = 0 if a then a = a + 7 end a()" 37 7)
	 (test "local a,e = 0 if a then a = a + 7 end a()" 39 7)
	 (test "function a () for i= 1,10 do return i end end a()" 47 10)
	 (test "local function a () for _,j in pairs{1,2} do return i end end a()" 63 16)
	 (test "local b local function a () return b end local function c () return b end" 69 7)
	 (test "local function a () end local function g () end a()" 49 16))))


(defun test-lexer ()
  (with-temp-buffer
	(insert "and  break  do  else  elseif  end  false  for  function  if  in  local  nil  not  or  
repeat  return  then  true  until  while some_id{}[]()+%*/, { } [ ] ( ) + % * / , end 
1 0.12 -1 -.1 .7 1e23 0xF 1.0e34 0xF 'string' # #hey dumb > < = == <= ; index")
	(loop with *token-start* = nil
		  with *token-end* = 1
		  with *token-type* = nil
		  for expected in '(and  break  do  else  elseif  end  false  for  function  if  in  
								 local  lua-nil  not  or  repeat  return  then  true  until  while id
								 lc rc lb rb lp rp + % * / comma
								 lc rc lb rb lp rp + % * / comma end
								 number number number number number number number number number
								 string pound pound id id > < = == <= semi id)
		  for token = (progn (next-lua-token *token-end*) (if (eq *token-type* 'white) 
														 (next-lua-token *token-end*)
													   *token-end*))
		  while token
		  if (not (eq *token-type* expected)) do (return)
		  finally (return t))))

(defun test-statement-lex ()
  (and (equal '(if id then end) (token-types "if id then end"))
	   (equal '(if pound id then end) (token-types "if #id then end"))
	   (equal '(if pound lp string rp > number then end) (token-types "if #('123') > 3 then end"))))

(defun run-lua-lex-tests ()
  (let ((failed nil))
	(with-current-buffer (get-buffer-create "*Test*")
	  (font-lock-mode)
	  (erase-buffer)
	  (loop for test in (list #'load-file-test 
							  #'test-lexer 
							  #'test-statement-lex
							  #'test-find-def)
			for test-error = nil
			for result = (condition-case e (funcall test)
						   (error (setq test-error e)))
			if test-error do (progn 
					  (insert (propertize (format "error:  %s - %s\n" test test-error)  'font-lock-face '(:foreground "red")))
					  (setq failed t))
			else if result do (insert (propertize (format "passed: %s\n" test) 'font-lock-face '(:foreground "green")))
			else do (progn 
					  (insert (propertize (format "failed: %s\n" test)  'font-lock-face '(:foreground "yellow")))
					  (setq failed t)))
	  (if failed (switch-to-buffer-other-window "*Test*")))))


