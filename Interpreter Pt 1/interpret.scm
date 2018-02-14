; #lang racket
; Interpreter Pt 1 EECS 345
; Group Members: Lee Radics, Zach Perlo, Rohan Krishna
; Case IDs:elr61, zip5, rxr353

(load "simpleParser.scm") ;Load simple parser to enable parser use

;The interpreter function used to parse/interpret the java-ish file (.javal)
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
      ;((eq? state "error") "error") ;return error if the state is error
      ((eq? (operator command) 'var) (declareHelper command state)) ;Variable declaration
      ((eq? (operator command) '=) (assignHelper command state)) ;Assign declaration
      ((eq? (operator command) 'if) (ifHelper command state)) ;if declaration
      ((eq? (operator command) 'while) (whileHelper command state)) ;while declaration
      ((eq? (operator command) 'return) (returnHelper command state)) ;return declaration
      (else (error 'badoperation "Invalid command")) ;standard throw error
      )
    )
  )

(define operator
  (lambda (lis)
    (car lis)
    )
  )

; Function to evaluate integer val of expression
; Example input: (+ a b) and state ((x y ...) (1 2 ...))
(Define MValue
        (lambda (expression state)
          (cond
            ((eq? (operator expression) '+) (+Helper expression state)) ; computing addition
            ((eq? (operator expression) '-) (-Helper expression state)) ; computing subtraction
            ((eq? (operator expression) '*) (*Helper expression state)) ; computing multiplication
            ((eq? (operator expression) '/) (/Helper expression state)) ; computing division
            ((eq? (operator expression) '%) (%Helper expression state)) ; computing mod
            (else (error 'badoperation "Invalid expression")) ;standard throw error
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

; function to look up a variable to get its value from the state
; Example input: y and state ((x y ...) (1 2 ...)) would return 2
(define lookup
  (lambda (var state)
    (lookup_helper2 (lookup_helper var (car state)) (cdr state))
    )
  )

; returns the index the variable is stored in state
(define lookup_helper
  (lambda (var stateVars)
    (cond
      ((null? stateVars) (error 'badoperation "Variable not defined"))
      ((eq? var (car stateVars)) 1)
      (else (+ 1 (lookup_helper (var (cdr stateVars)))))
      )
    )
  )

; returns value at index x in state
(define lookup_helper2
  (lambda (x stateNums)
    (cond
      ((null? stateNums) (error 'badoperation "Variable not defined"))
      ((AND (eq? x 1) (eq? (car stateNums) ; NULL value for vars )) (error 'badoperation "Variable not initialized"))
      ((eq? x 1) (car stateNums))
      (else (lookup_helper2 (- x 1) (cdr stateNums)))
      )
    )
  )

    
; function to replace a variable's current value
; Example input: y, 5, and state ((x y ...) (1 2 ...)) would return state ((x y ...) (1 5 ...))
(define replaceVar
  (lambda (var newVal state)
    (cons (replace_helper (lookup_helper var (car state)) newVal (cdr state)))
    )
  )

; replaces value at index x with newVal in stateNums (returns stateNums)
(define replace_helper
  (lambda (x newVal stateNums)
    (cond
      ((null? stateNums) (error 'badoperation "Variable not defined"))
      ((eq? x 1) (cons newVal (cdr stateNums)))
      (else (cons (car stateNums) (replace_helper (- x 1) newVal (cdr stateNums))))
      )
    )
  )

;=====================;
;MState Helper Methods
;=====================;

; declare helper
(define declareHelper
  (lambda (command state)
    )
  )

; assign helper
(define assignHelper
  (lambda (command state)
    )
  )

; if helper
(define ifHelper
  (lambda (command state)
    )
  )

; while helper
(define whileHelper
  (lambda (command state)
    )
  )

; return helper
(define returnHelper
  (lambda (command state)
    )
  )

;=====================;
;MValue Helper Methods
;=====================;

(define operand1
  (lambda (lis)
    (cadr lis)
    )
  )

(define operand2
  (lambda (lis)
    (caddr lis)
    )
  )

; addition helper
(define +Helper
  (lambda (expression state)
    (cond
      ((null? expression) (error 'badoperation "Invalid expression")) 
      ((null? (cdr expression)) (error 'badoperation "Invalid expression"))
      ((null? (cdr (cdr expression))) (error 'badoperation "Invalid expression"))
      ((AND (list? operand1) (list? operand2)) (+ (MValue operand1 state) (MValue operand1 state))) ;MValue because you will only
      ((list? operand1) (+ (MValue operand1 state) operand2))                                       ;add a number to a number
      ((list? operand2) (+ operand1 (MValue operand2 state)))
      (else (+ operand1 operand2))
      )
    )
  )

; subtraction helper
(define -Helper
  (lambda (expression state)
    (cond
      ((null? expression) (error 'badoperation "Invalid expression")) 
      ((null? (cdr expression)) (error 'badoperation "Invalid expression"))
      ((null? (cdr (cdr expression))) (error 'badoperation "Invalid expression"))
      ((AND (list? operand1) (list? operand2)) (+ (MValue operand1 state) (MValue operand1 state))) ;MValue because you will only
      ((list? operand1) (- (MValue operand1 state) operand2))                                       ;add a number to a number
      ((list? operand2) (- operand1 (MValue operand2 state)))
      (else (- operand1 operand2))
      )
    )
  )

; multiplication helper
(define *Helper
  (lambda (expression state)
    (cond
      ((null? expression) (error 'badoperation "Invalid expression")) 
      ((null? (cdr expression)) (error 'badoperation "Invalid expression"))
      ((null? (cdr (cdr expression))) (error 'badoperation "Invalid expression"))
      ((AND (list? operand1) (list? operand2)) (* (MValue operand1 state) (MValue operand1 state))) ;MValue because you will only
      ((list? operand1) (* (MValue operand1 state) operand2))                                       ;add a number to a number
      ((list? operand2) (* operand1 (MValue operand2 state)))
      (else (* operand1 operand2))
      )
    )
  )

; division helper
(define /Helper
  (lambda (expression state)
    (cond
      ((null? expression) (error 'badoperation "Invalid expression")) 
      ((null? (cdr expression)) (error 'badoperation "Invalid expression"))
      ((null? (cdr (cdr expression))) (error 'badoperation "Invalid expression"))
      ((AND (list? operand1) (list? operand2)) (quotient (MValue operand1 state) (MValue operand1 state))) ;MValue because you will only
      ((list? operand1) (quotient (MValue operand1 state) operand2))                                       ;add a number to a number
      ((list? operand2) (quotient operand1 (MValue operand2 state)))
      (else (quotient operand1 operand2))
      )
    )
  )

; mod helper
(define %Helper
  (lambda (expression state)
    (cond
      ((null? expression) (error 'badoperation "Invalid expression")) 
      ((null? (cdr expression)) (error 'badoperation "Invalid expression"))
      ((null? (cdr (cdr expression))) (error 'badoperation "Invalid expression"))
      ((AND (list? operand1) (list? operand2)) (remainder (MValue operand1 state) (MValue operand1 state))) ;MValue because you will only
      ((list? operand1) (remainder (MValue operand1 state) operand2))                                       ;add a number to a number
      ((list? operand2) (remainder operand1 (MValue operand2 state)))
      (else (remainder operand1 operand2))
      )
    )
  )

;=====================;
;MBool Helper Methods
;=====================;

; equality helper
(define ==Helper
  (lambda (expression state)
    )
  )

; inequality helper
(define !=Helper
  (lambda (expression state)
    )
  )

; greater than helper
(define >Helper
  (lambda (expression state)
    )
  )

; less than helper
(define <Helper
  (lambda (expression state)
    )
  )

; greater than or equal to helper
(define >=Helper
  (lambda (expression state)
    )
  )

; less than or equal to helper
(define <=Helper
  (lambda (expression state)
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
       (cons (cons (car (cdr command)) (car state)) (cons (cons "undefined" (car (cdr state))) '())))
      ((eq? (MValue (cons (car (cdr (cdr command))) '()) state) "error");if the state is an error
       (cons (cons (car (cdr command)) (car state)) (cons (cons (MBoolean (cons (car (cdr (cdr command))) '()) state) (car (cdr state))) '())))
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
      (else (cons (car (car (cdr state))) (updateList command (cons (cdr (car state)) (cons (cdr (car (cdr state))) '())))))
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