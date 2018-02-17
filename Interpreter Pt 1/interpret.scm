; Interpreter Pt 1 EECS 345
; Group Members: Lee Radics, Zach Perlo, Rohan Krishna
; Case IDs:elr61, zip5, rxr353

(load "simpleParser.scm") ;Load simple parser to enable parser use

;The interpreter function used to parse/interpret the javaish file (.javaish)
(define interpret
  (lambda (filename)
    (evaluate (parser filename) '(()()));passes the filename to evaluate/parse program tree
    )
  )
  

;Function to evaluate and step through program tree
(define evaluate
  (lambda (prgm state)
    (cond
      ((eq? state #t) "true") ;if prgm null and equals state true
      ((eq? state #f) "false")
      ((number? state) state)
      ((null? prgm) "Error, no return") ;now finally check if prgm is null, then return state
      (else (evaluate (cdr prgm) (MState (car prgm) state))) ;evaluate filename while calling MState on the command
      )                                                      ;if checks are passed 
    )
  )

;Function to return program state after command is run
;Commands: var, =, if, while, return and the state is ((x y ...) (1 2 ...))
(define MState
  (lambda (command state)
    (cond
      ((eq? state "error") "error") ;return error if the state is error
      ((eq? (operator command) 'var) (declareHelper command state)) ;Variable declaration
      ((eq? (operator command) '=) (assignHelper command state)) ;Assign declaration
      ((eq? (operator command) 'if) (ifHelper command state)) ;if declaration
      ((eq? (operator command) 'while) (whileHelper command state)) ;while declaration
      ((eq? (operator command) 'return) (returnHelper command state)) ;return declaration
      (else (error 'badoperation "Invalid command")) ;standard throw error
      )
    )
  )

; Function to evaluate integer val of expression
; Example input: (+ a b) and state ((x y ...) (1 2 ...))
; Returns error on any weirdness
(define MValue
        (lambda (expression state)
          (cond
            ((list? (car expression))
             (MValue (car expression) state))

           ;The operand expressions
           ((and (eq? (car expression) '+) ;addition expression
                 (not (or (eq? (MValue (cdr expression) state) "error")
                          (eq? (MValue (cdr (cdr expression)) state) "error"))))
           (+ (MValue (cdr expression) state) (MValue (cdr(cdr expression)) state)))
           ((and (eq? (car expression) '-) ;standard subtraction
                 (and (not (eq? (MValue (cdr expression) state) "error"))
                      (null? (cdr (cdr expression)))))
           (- 0 (MValue (cdr expression) state)))
           ((and (eq? (car expression) '-) ;second subtraction statement
                 (not (or (eq? (MValue (cdr expression) state) "error")
                          (eq? (MValue (cdr (cdr expression)) state) "error"))))
           (- (MValue (cdr expression) state) (MValue (cdr(cdr expression)) state)))
           ((and (eq? (car expression) '*) ;Multiplication statement, I'm starting to hate these
                 (not (or (eq? (MValue (cdr expression) state) "error")
                          (eq? (MValue (cdr (cdr expression)) state) "error"))))
           (* (MValue (cdr expression) state) (MValue (cdr(cdr expression)) state)))
           ((and (eq? (car expression) '/) ;Division statement
                 (not (or (eq? (MValue (cdr expression) state) "error")
                          (eq? (MValue (cdr (cdr expression)) state) "error"))))
           (quotient (MValue (cdr expression) state) (MValue (cdr(cdr expression)) state)))
           ((and (eq? (car expression) '%) ;final modulo statement
                 (not (or (eq? (MValue (cdr expression) state) "error")
                          (eq? (MValue (cdr (cdr expression)) state) "error"))))
           (remainder (MValue (cdr expression) state) (MValue (cdr(cdr expression)) state)))

           ((number? (car epression)) (car expression))
           ((listCheck (car expression) state) (lookup (car expression) state))
           (else "error")
            )
          )
        )

; Function to evaluate truth val of expression
; Example input: (== x y) and state ((x y ...) (1 2 ...))
(define MBool
        (lambda (condition state)
          (cond
            ((list? (car condition))
             (MBool (car condition) state))
            ((and (eq? (car condition) '==) ;equality checker
                  (not (or (eq? (MValue (cdr condition) state) "error")
                           (eq? (MValue (cdr(cdr condition)) state) "error"))))
      (eq? (MValue (cdr condition) state) (MValue (cdr(cdr condition)) state)))
            ((and (eq? (car condition) '!=) ;inequality checker
                  (not (or (eq? (MValue (cdr condition) state) "error")
                           (eq? (MValue (cdr(cdr condition)) state) "error"))))
      (not(eq? (MValue (cdr condition) state) (MValue (cdr(cdr condition)) state))))
            ((and (eq? (car condition) '>) ;greater than checker
                  (not (or (eq? (MValue (cdr condition) state) "error")
                           (eq? (MValue (cdr(cdr condition)) state) "error"))))
      (> (MValue (cdr condition) state) (MValue (cdr(cdr condition)) state)))
            ((and (eq? (car condition) '<) ;lesser than checker
                  (not (or (eq? (MValue (cdr condition) state) "error")
                           (eq? (MValue (cdr(cdr condition)) state) "error"))))
      (< (MValue (cdr condition) state) (MValue (cdr(cdr condition)) state)))
            ((and (eq? (car condition) '>=) ;greater than/equal to checker
                  (not (or (eq? (MValue (cdr condition) state) "error")
                           (eq? (MValue (cdr(cdr condition)) state) "error"))))
      (>= (MValue (cdr condition) state) (MValue (cdr(cdr condition)) state)))
            ((and (eq? (car condition) '<=) ;lesser than/equal to checker
                  (not (or (eq? (MValue (cdr condition) state) "error")
                           (eq? (MValue (cdr(cdr condition)) state) "error"))))
      (<= (MValue (cdr condition) state) (MValue (cdr(cdr condition)) state)))
            ((and (eq? (car condition) '&&) ;and checker
                  (not (or (eq? (MBoolean (cdr condition) state) "error")
                           (eq? (MBoolean (cdr(cdr condition)) state) "error"))))
      (and (MBoolean (cdr condition) state) (MBoolean (cdr(cdr condition)) state)))
            ((and (eq? (car condition) '||) ;or checker
                  (not (or (eq? (MBoolean (cdr condition) state) "error")
                           (eq? (MBoolean (cdr(cdr condition)) state) "error"))))
      (or (MBoolean (cdr condition) state) (MBoolean (cdr(cdr condition)) state)))
            ((and (eq? (car condition) '!)
                  (not (eq? (MBoolean (cdr condition) state) "error")))
      (not (MBoolean (cdr condition) state)))
            ((eq? (car condition) 'true)  #t)
            ((eq? (car condition) 'false)  #f)
            ((eq? (car condition) '#t) #t)
            ((eq? (car condition) '#f) #f)
            ((listCheck (car condition) state)
             (if (or (eq? (lookup (car condition) state) 'true)
                     (eq? (lookup (car condition) state) '#t)) #t
                          (if (or (eq? (lookup (car condition) state) 'false)
                              (eq? (lookup (car condition) state) '#f)) #f "error")))
            )
          )
        )
                                     

;=====================;
;MState Helper Methods
;=====================;

; declare helper
; adds element to state if not declared
; Takes (= z 4) and state ((x y ...) (1 2 ...))
(define declareHelper
  (lambda (command state)
    (cond
      ((decCheck command (car state))
       (addL command state)) ;returns state with new variable added if command valid
      (else "error");error on wierdness
      )
    )
  )

; assign helper
; Assigns value to an element in the state
; Takes (= z 4) and state ((x y ...) (1 2 ...))
(define assignHelper
  (lambda (command state)
    ((listCheck (cadr command) state) ;returns state with var value if valid command
     (cons (car state) (cons (updateL (cdr command) state) '())))
    (else "error")
    )
  )


; if helper
; takes command (if bool true-expr false-expr) (false-expr optional) and state  ((x y) (1 2) ...))
(define ifHelper
  (lambda (command state)
    (cond
      ((MBool (car (cdr command)) state)
       (MState (car (cdr (cdr command))) state));returns state of program after correct statement is run
      ((null? (cdr (cdr (cdr command)))) state)
      (else (MState (car (cdr (cdr (cdr command)))) state))
      )
    )
  )

; while helper
; Runs for multiple iterations until condition is no longer true
; takes command (while bool body) and state ((x y ...) (1 2 ...))
(define whileHelper
  (lambda (command state)
    (cond
      ((null? (cdr (cdr command)))
       state)
      ((MBool (car (cdr command)) state)
       (whileHelper command (MState (cdr (cdr command)) state)))
      (else state)
      )
    )
  )

; return helper
; returns integer value of command if error not returned
; Takes (return (+ z 4)) and state ((x y ...) (1 2 ...))
(define returnHelper
  (lambda (command state)
    (cond
      ((not (eq? (MValue (cdr command) state) "error"))
       (MValue (cdr command) state)) ;Returns int value of command if MValue doesn't return "error"
      (else (MBool (cdr command) state))
      )
    )
  )


;======================;
;Other Helper Methods
;======================;
; returns #t if the variable being assigned has not been declared
; takes assignment command (= z 6) and state-vars (x y z ...) aka (car state)
(define decCheck
  (lambda (command state-vars)
    (cond
      ((null? state-vars)
       #t);null state
      ((eq? (car state-vars) (car(cdr command)))
       #f);returns #f if variable is declared
      (else (decCheck command (cdr state-vars)))
      )
    )
  )

; returns #t if the variable has been declared (is in the state)
(define listCheck
  (lambda (var state)
    (cond
      ((null? state)
       #f);null state
      ((null? (car state))
       #f);expression null
      ((eq? var (car (car state)))
       #t);true if declared
      (else (listCheck var (cons (cdr (car state)) (cons (cdr (car (cdr state))) '()))))
      )
    )
  )

; Adds the variable and value
; takes command (var x expr) (expr optional) and state ((z y ...) (1 2 ...))
(define addL
  (lambda (var state)
    (cond
      ((null? (cdr (cdr command))); to check if expressions has been passed
       (cons (cons (car (cdr command)) (car state))
             (cons (cons "undefined" (car (cdr state))) '())))
      ((eq? (MValue (cons (car (cdr (cdr command))) '()) state) "error");if the state is an error
       (cons (cons (car (cdr command)) (car state))
             (cons (cons (MBool (cons (car (cdr (cdr command))) '()) state)
                         (car (cdr state))) '())))
      (else (cons (cons (car (cdr command)) (car state)) (cons (cons (MValue (cons (car (cdr (cdr command))) '()) state) (car (cdr state))) '())))
      )
    )
  )

; updates the state with the variable-value pair
; takes command (x expr) and state ((x y ...) (1 2 ...))
(define updateL
  (lambda (command state)
    (cond
      ((null? (car state))
       #f) ; Should never get here because we already checked to see if variable can be added to list
      ((eq? (car command) (car (car state)))
       (cons (MValue (cons (car (cdr command)) '()) state) (cdr (car (cdr state)))))
      (else (cons (car (car (cdr state)))
                  (updateL command (cons (cdr (car state))
                                            (cons (cdr (car (cdr state))) '())))))
    )
  )
  )
  
; Returns the value of the variable as stored in the state
; takes var as a variable name and state ((x y ...) (1 2 ...))
(define lookup
  (lambda (var state)
    (cond
      ((null? (car state))
       "error");returns error if x isn't in the state
      ((eq? (car (car state)) var)
       (car (car (cdr state))));standard find method
      (else (lookup var (cons (cdr (car state)) (cons (cdr (car (cdr state))) '()))))
      )
    )
  )