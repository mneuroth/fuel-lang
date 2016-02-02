(do

  ;(import fuellib)
    
  (defn f (x) 
     (do
        (gdefn g (x)
            (* x x x)
        )

        (print x)
        (+ x x (g 4))
     )  
  )

  ; Neues define-macro-expand --> erzeugt Code zur Compile/MacroExpand Zeit ?

  (defn foreach (container fcn) 
     (do 
	   (def i 0)
	   (def max (len container))
	   (while (< i max)
	     (do 
		   (apply fcn (list (nth i container)))
		   (setf i (+ i 1))
		 )
	   )
	 )
  )
  
  (define-macro-expand foreach-macro
        (container fcn) 
        (do 
    	   (def i 0)
    	   (def max (len container))
    	   (while (< i max)
    	     (do 
    		   (apply fcn (list (nth i container)))
    		   (setf i (+ i 1))
    		 )
    	   )        
        )
  )

  (defn make-args-string (count) 
    (do 
	  (def s "this ")
	  (def i 0)
	  (while (< i count)
	    (do 
			(setf s (+ s "p" (string i)))
			(setf s (+ s " "))
			(setf i (+ i 1))
		)
	  )
	  (return s)
	)
  )

  (defn make-args (count) 
    (do 
	  (def s "this ")
	  (def i 0)
	  (while (< i count)
	    (do 
			(setf s (+ s "p" (string i)))
			(setf s (+ s " "))
			(setf i (+ i 1))
		)
	  )
	  (return s)
	)
  )

  (defn make-args-no-this (count) 
    (do 
	  (def s "")
	  (def i 0)
	  (while (< i count)
	    (do 
			(setf s (+ s "p" (string i)))
			(setf s (+ s " "))
			(setf i (+ i 1))
		)
	  )
	  (return s)
	)
  )

  (define-macro create-native-old 
      (lambda (lisp-name full-class-name) 
        (do
            ; inspect class and generate for all public properties and methods method stubs
            (gdefn (sym (string "create-" lisp-name)) () (call full-class-name))
			
			(def methods (native-methods full-class-name))
			(print "length=" (len methods))
			(foreach methods (lambda (methodNameAndArgCount) 
			   (do 
			     (def methodName (first methodNameAndArgCount))
			     (def methodArgsCount (nth 1 methodNameAndArgCount))
			     (print "create method=" methodName methodCount (make-args-string methodArgsCount))

				 (if (= methodArgsCount 0)
				   (gdefn (sym (string lisp-name "-" methodName)) (this) (call this methodName)) 
				   (nop))
				 (if (> methodArgsCount 0)
				   (gdefn 
					  (sym (string lisp-name "-" methodName))          
						(macro-expand (make-args methodArgsCount))      
; todo --> list flach klopfen !
						(call (append (list this methodName) (macro-expand (make-args-no-this methodArgsCount))))  
		           )
				   ;(gdefn (sym (string lisp-name "-" methodName)) (this p1) (call this methodName p1)) 
				   (nop))  			     
			   )
			))
        ) 
     )
  )

  (define-macro-expand create-method (method-name arg-count) 
      (do
	      (def args (make-args-no-this arg-count))
          (return `(gdefn method-name ,@args))
	  )
  )
  
  ;; Macro --> code waehrend compilier/parse phase ausfuehren/evaluieren und resultat der evaluation als code interpretieren
  
  ;; create list of symbols: (call obj methodName p1 p2 p3 p4)
  (defn create-call (methodName argCount)
      (do
         (def _result (list))
         (setf _result (append _result (list 'call)))
         (setf _result (append _result (list 'obj)))
         (setf _result (append _result (list (str methodName))))
         (def i 0)
         (while (< i argCount)
           (do 
             (setf _result (append _result (list (+ "p" (string i)))))
  			 (setf i (+ i 1))
           )
         )
         (return _result)
      )
  )

  ;; create list of symbols count=4 --> (p1 p2 p3 p4)
  (defn arg-list (argCount)
     (do
         (def result (list))
         (def i 0)
         (while (< i argCount)
           (do 
             (setf result (append result (list (+ "p" (string i)))))
  			 (setf i (+ i 1))
           )
         )
         (return result)
     )
  )

  ;; create list of symbols count=4 --> (obj p1 p2 p3 p4)
  (defn obj-arg-list (argCount)
     (do
         (def result (list))
         (setf result (append result (list 'obj)))
         (setf result (append result (arg-list argCount)))
         (return result)
     )
  )

  (define-macro-expand create-native 
      (lisp-name full-class-name) 
        (do
            ; inspect class and generate for all public properties and methods method stubs
            (gdefn (sym (string "create-" lisp-name)) () (call full-class-name))
			
			(def methods (native-methods full-class-name))
			(print "length=" (len methods))
;; TODO gulp --> hier geht anscheinend etwas mit dem Stack daneben, die Variable methods ist im foreach-macro NICHT mehr auf dem Stack wo sie sein sollte !!!            
			(foreach-macro methods (lambda (methodNameAndArgCount) 
			   (do 
                 (print "arg=" methodNameAndArgCount)  
			     (def methodName (first methodNameAndArgCount))
			     (def methodArgsCount (nth 1 methodNameAndArgCount))
			     (print "create method=" methodName methodCount (make-args-string methodArgsCount))

;;				 (if (= methodArgsCount 0)
;;				   (gdefn 
;;                       (sym (string lisp-name "-" methodName))      ; function name
;;                       (this)                                       ; formal arguments
;;                       (call this methodName))                      ; code
;;				   (nop))  ; else
				 (if (>= methodArgsCount 0)
                   (do
  				     (gdefn 
					    (sym (string lisp-name "-" methodName))     ; function name
                        (obj-arg-list methodArgsCount)              ; formal arguments
                        ; make something like: (call obj "Function-Name" argument1 argument2 ...)
				        (create-call methodName methodArgsCount)    ; code
		             )                   
                   )
				   (nop)) ; else
			   )
			))
        )     
  )
  
  (defn test_macro (x lisp-name methodName methodArgsCount) 
     (do
         
        (gdefn macro_fcn (x)
            (* x x x)
        )
        
	    (gdefn 
		  (sym (string lisp-name "-" methodName))          
          (a b c)
          (+ a b c)
        )
       
        (print "test_macro" x)
     )  
  )

  (print "test macro...")
  (test_macro 7 "List" "blub" 3)
        (print List-blub 1 2 3)
        (print (List-blub 1 2 3))
  (print "done.")
  
;;  (print (make-args-string 8))
;;  (def s1 "blub")
;;  (def s2 "hallo")
;;  (def sum (+ s1 s2))
;;  (print blubhallo)
;;  (print "sum=" sum)
;;  (print "s1=" s1)
;;  (print "s2=" s2)
;;  (print (f 5))
;;  (print (g 4))
;;  (print "try macro...")

  (gdefn (sym (string "create-" "ListX")) () (call "System.Collections.Generic.List`1[[System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]"))
  (print "....")
  (create-native "List" "System.Collections.Generic.List`1[[System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]")
  (print "---")
  (print "(1)")
  (def obj (create-List))
  (print "---" obj)
  (print (native-methods obj))
  (print "try add...")
  (call obj "Add" 5)
  (call obj "Add" 6)
  (print "added elements phase 1")
;  (print (arg-list 8))
;  (print (create-call "test" 4))
;;TODO  
;  (print (obj-arg-list 8))
  (List-Add obj 7)
  (print "added elements")
  (List-get_Count obj)
;;  (print "added elements #2")
;;  (print (call obj "get_Count"))
;;  (print "(2)")
;;  (print obj)

)