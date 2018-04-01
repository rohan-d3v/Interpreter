;EECS 345 Interpreter pt 3
;Team Members:	Rohan Krishna, Zach Perlo, Lee Radics
;Case IDs:		rxr353, zip5, 

;This Code was re-written/ restructured using a solution2.scm from Canvas to help clean up MState etc.

(load "functionParser.scm")


; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
;(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment.
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.

(define interpret
  (lambda (file)
    (call/cc
      (lambda (return)
        (let* ((initial-return (lambda (statement env) 
        	(return (Mvalue (operand statement) 
        		env return default-break default-continue default-throw))))

               (outer-environment (do-interpret (parser file) initEnv 
               	(lambda (statement env) (return env)) 
               	default-break default-continue default-throw))

               (begin-interpret (lambda (env) 
               	(Mvalue-funcall (mainFuncall main) 
               		env initial-return default-break default-continue default-throw))))

              ; Begin interpreting. Pass in the environment, which is built by interpreting the outermost layer
              ; of the program, containing function and global variable definitions.
              (begin-interpret 
              	(getFunctionExecutionEnvironment (mainFuncall main) 
              		outer-environment initial-return default-break default-continue default-throw)))
        )
      )
    )
  )


(define main '((funcall main)))
(define mainFuncall car)

; do-interpret recursively evaluates statements
(define do-interpret
  (lambda (statement state return break continue throw)
    (if (null? statement)
      state
      (do-interpret (restOfExpressions statement)
                    (Mstate (firstExpression statement) state return break continue throw)
                    return break continue throw)
      )
    )
  )

(define initEnv '(((true false) (true false))))
(define default-break 
	(lambda (s) 
	(error 'invalidBreak "Break was incorrect")))
(define default-continue 
	(lambda (s) 
		(error 'invalidContinue "Continue was incorrect")))
(define default-throw 
	(lambda (e s) 
		(error 'uncaughtException "Uncaught exception")))

; interpret a statement in the environment with continuations for return, break, continue, throw
(define Mstate
  (lambda (statement state return break continue throw)
    (cond
      ((eq? (operator statement) '=) (assignHelper statement state return break continue throw))
      ((eq? (operator statement) 'begin) (beginHelper (cdr statement) state return break continue throw))
      ((eq? (operator statement) 'break) (break state))
      ((eq? (operator statement) 'continue) (continue state))
      ((eq? (operator statement) 'funcall) (funcallHelper statement state return break continue throw))
      ((eq? (operator statement) 'function) (Mstate-func statement state))
      ((eq? (operator statement) 'if) (Mstate-if statement state return break continue throw))
      ((eq? (operator statement) 'return) (return statement state))
      ((eq? (operator statement) 'throw) (throw (Mvalue (exception statement) state return break continue throw)))
      ((eq? (operator statement) 'try) (Mstate-tcf statement state return break continue throw))
      ((eq? (operator statement) 'var) (Mstate-var statement state return break continue throw))
      ((eq? (operator statement) 'while)
        (call/cc
          (lambda (new-break)
            (Mstate-while (parse-while-condition statement) (parse-while-statement statement) state return new-break continue throw))))
      (else (error 'unknown "Encountered an unknown statement")))))

(define exception cadr)

;=======================;
;Mstate Helper Functions;
;=======================;

; assignHelper handles variable assignment
(define assignHelper
  (lambda (statement env r b c t)
    (replace_var (variable statement) (Mvalue (operation statement) env r b c t) env)))

; Whenever entering a block of code with curly braces, this function should be called to evaluate
; the contents of the block inside a new layer of scope.
; Statement format:
; (begin (stmt-1) (stmt-2) ...)
(define beginHelper
  (lambda (statement env return break continue throw)
    (getInnerScope (do-interpret statement
                                 (addLevelOfScope env)
                                 return
                                 (lambda (s) (break (getInnerScope s)))
                                 (lambda (s) (continue (getInnerScope s)))
                                 throw))))

; Mstate-if handles if statements
; Statement format: (else-statement is optional)
; (if (condition) (statement) (else-statement))
(define Mstate-if
  (lambda (statement env return break continue throw)
    (cond
      ((eq? 'true (Mbool (if-condition statement) env return break continue throw)) (Mstate (if-statement statement) env return break continue throw))
      ((not (null? (else-statement-exists statement))) (Mstate (else-statement statement) env return break continue throw))
      (else env))))

(define else-statement-exists cdddr)
(define if-condition cadr)
(define if-statement caddr)
(define else-statement cadddr)

; Mstate-func handles function declarations
; Statement format:
; (function function-name (formal-param-1, formal-param-2, ...) (body))
(define Mstate-func
  (lambda (statement env)
    (cond
      ((stateContains (funcName statement) env) (error 'redefining (format "function ~a has already been declared" (funcName statement))))
      (else (insert (funcName statement) (createClosure (getParams statement) (getBody statement)) env)))))

;helper methods for Mstate-func
(define funcName cadr)
(define getParams caddr)

; When a function is called without the calling line needing its return
; value, execute the function and then return the environment.
; Statement format:
; (funcall function-name actual-param-1 actual-param-2 ...)
(define funcallHelper
  (lambda (funcall env return break continue throw)
    (begin (Mvalue-funcall funcall env return break continue throw) env)))

;helpers for funcallHelper
(define globalStateOfEnvironment cdr)
(define getFuncBody cadr)
(define getFuncEnvironment caddr)
(define getBody cadddr)

; Modify the state based on a try-catch-finally block.
; Statement format, where each "body" can consist of multiple statements in a list:
; (try (try-body) (catch (exception-name) (catch-body)) (finally (finally-body)))
(define Mstate-tcf
  (lambda (statement env return break continue throw)
    (call/cc
      (lambda (catch-continuation)
        (letrec ((finally (lambda (s)
                  (if (pair? (finally-stmt statement))
                      (beginHelper (finally-body statement) s return break continue throw)
                      s)))
                (try (lambda (new-throw)
                  ; if this try block is accompanied by a catch block, pass a continuation that
                  ; jumps us to it when we encounter a throw. Otherwise, pass whatever throw continuation
                  ; we were passed when we entered this try block.
                  (if (pair? (catch-block statement))
                    (finally (beginHelper (try-body statement) env return break continue new-throw))
                    (finally (beginHelper (try-body statement) env return break continue throw)))))
                (catch (lambda (e s)
                  (finally (catch-begin (catch-body statement) (catch-err statement) e s return break continue throw)))))
                ; Call "try" with catch as the catch-continuation
                (try (lambda (e) (catch-continuation (catch e env)))))))))

; Same as beginHelper, but with the addition of inserting the exception into the
; environment before calling do-interpret.
(define catch-begin
  (lambda (statement e-name e-value env return break continue throw)
    (getInnerScope (do-interpret statement
                                 (insert e-name e-value (addLevelOfScope env))
                                 return
                                 (lambda (s) (break (getInnerScope s)))
                                 (lambda (s) (continue (getInnerScope s)))
                                 throw))))

(define try-body cadr)
(define catch-body (lambda (v) (caddr (caddr v))))
(define catch-block caddr)
(define catch-err (lambda (v) (car (cadr (caddr v)))))
(define finally-stmt (lambda (t) (car (cdddr t))))
(define finally-body (lambda (t) (cadr (car (cdddr t)))))

; MState-var handles variable declaration
; Statement format:
; (var var-name) OR (var var-name value)
(define Mstate-var
  (lambda (statement env r b c t)
    (cond
      ;((stateContains (variable statement) env) (error 'redefining (format "Variable ~a has already been declared" (variable statement))))
      ((null? (thirdElement statement)) (insert (variable statement) 'undefined env))
      (else (insert (variable statement) (Mvalue (operation statement) env r b c t) env)))))

; Mstate-while handles while loops
; TODO: check that continue actually works
; Statement format:
; (while (condition) (body))
; body may be one line only; for multiple lines, it must contain a begin.
(define Mstate-while
  (lambda (condition statement env return break continue throw)
    (if (eq? 'true (Mbool condition env return break continue throw))
      (Mstate-while condition
                    statement
                    (call/cc
                      (lambda (new-continue)
                        (Mstate statement env return break new-continue throw)))
                    return
                    break
                    continue
                    throw)
      env)))

(define parse-while-condition cadr)
(define parse-while-statement caddr)