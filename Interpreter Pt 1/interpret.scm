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
      ((and (null? prgm) (eq? state #t)) "true") ;if prgm null and equals state true
      ((and (null? prgm) (eq? state #f)) "false")
      ((null? prgm) state) ;now finally check if prgm is null, then return state
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
; Returns error on any wierdness
(Define MValue
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
           (floor (/ (MValue (cdr expression) state) (MValue (cdr(cdr expression)) state))));floor used to determine the higher value
           ((and (eq? (car expression) '%) ;final modulo statement
                 (not (or (eq? (MValue (cdr expression) state) "error")
                          (eq? (MValue (cdr (cdr expression)) state) "error"))))
           (modulo (MValue (cdr expression) state) (MValue (cdr(cdr expression)) state)))

           ((number? (car epression)) (car expression))
           ((isInList (car expression) state) (lookup (car expression) state))
           (else "error")
            )
          )
        )

; Function to evaluate truth val of expression
; Example input: (== x y) and state ((x y ...) (1 2 ...))
(Define MBool
        (lambda (expression state)
          (cond
            ((eq? (operator expression) '==) (==Helper expression state)) ; checking equality
            ((eq? (operator expression) '!=) (!=Helper expression state)) ; checking inequality
            ((eq? (operator expression) '>) (>Helper expression state)) ; checking greater than
            ((eq? (operator expression) '<) (<Helper expression state)) ; checking less than
            ((eq? (operator expression) '>=) (>=Helper expression state)) ; checking greater than or equal to
            ((eq? (operator expression) '<=) (<=Helper expression state)) ; checking less than or equal to
            (else (error 'badoperation "Invalid expression")) ;standard throw error
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
      ((firstVarNotDeclared command (car state))
       (addToList command state)) ;returns state with new variable added if command valid
      (else "error");error on wierdness
      )
    )
  )

; assign helper
; Assigns value to an element in the state
; Takes (= z 4) and state ((x y ...) (1 2 ...))
(define assignHelper
  (lambda (command state)
    ((isInList (car (cdr command)) state) ;returns state with var value if valid command
     (const (car state) (cons (updateList (cdr command) state) '())))
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
; Runs the for the multiple iterations until condition is met
; takes command (while bool body) and state ((x y ...) (1 2 ...))
(define whileHelper
  (lambda (command state)
    (cond
      ((MBool (car (cdr command)) state);returns program state after all loops are run
       (MState (car (cdr (cdr command))) state))
      ((null? (cdr (cdr (cdr command))))
       state)
      (else (MState (car (cdr (cdr (cdr command)))) state))
      )
    )
  )

; return helper
; returns integer value of command if error not returned
; Takes (= z 4) and state ((x y ...) (1 2 ...))
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
(define firstVarNotDeclared
  (lambda (command state-vars)
    (cond
      ((null? state-vars)
       #t);null state
      ((eq? (car state-vars) (car(cdr command)))
       #f);returns #f if variable is declared
      (else (firstVarNotDeclared command (cdr state-vars)))
      )
    )
  )

; returns #t if the variable has been declared (is in the state)
(define isInList
  (lambda (var state)
    (cond
      ((null? state)
       #f);null state
      ((null? (car state))
       #f);expression null
      ((eq? var (car (car state)))
       #t);true if declared
      (else (isInList var (cons (cdr (car state)) (cons (cdr (car (cdr state))) '()))))
      )
    )
  )

; Adds the variable and value
; takes command (var x expr) (expr optional) and state ((z y ...) (1 2 ...))
(define addToList
  (lambda (var state)
    (cond
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
  )

; updates the state with the variable-value pair
; takes command (x expr) and state ((x y ...) (1 2 ...))
; this is a contingency method. You should never have to use this
(define updateList
  (lambda (command state)
    (cond
      ((null? (car state))
       #f) ; Should never get here because we already checked to see if variable can be added to list
      ((eq? (car command) (car (car state)))
       (cons (MValue (cons (car (cdr command)) '()) state) (cdr (car (cdr state)))))
      (else (cons (car (car (cdr state)))
                  (updateList command (cons (cdr (car state))
                                            (cons (cdr (car (cdr state))) '())))))
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