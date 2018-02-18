; Interpreter Pt 1 EECS 345
; Group Members: Lee Radics, Zach Perlo, Rohan Krishna
; Case IDs: elr61, zip5, rxr353

(load "simpleParser.scm") ; Load simple parser to enable parser use

; The interpreter function used to parse/interpret the javaish file (.javaish)
(define interpret
  (lambda (filename)
    (evaluate (parser filename) initialState) ; passes the filename to evaluate/parse program tree
    )
  )
  
(define initialState '(()())) ; The starting value of the state

; Function to evaluate and step through program tree
(define evaluate
  (lambda (prgm state)
    (cond
      ((eq? state "error") "error") ; if return was called with an error
      ((eq? state #t) "true") ; if return was called with the value of true
      ((eq? state #f) "false") ; if return was called with the value of false
      ((number? state) state) ; if return was called with a numerical value
      ((null? (cdr prgm)) state) ;now finally check if prgm is null, then return state
      (else (evaluate (cdr prgm) (MState (car prgm) state))) ; evaluate filename while calling MState on the command
      )                                                      ; if checks are passed 
    )
  )

; Function to return program state after command is run
; Commands: var, =, if, while, return and the state is ((x y ...) (1 2 ...))
(define MState
  (lambda (command state)
    (cond
      ((eq? state "error") "error") ; return error if the state is error
      ((eq? (operator command) 'var) (declareHelper command state)) ; Variable declaration
      ((eq? (operator command) '=) (assignHelper command state)) ; Assign declaration
      ((eq? (operator command) 'if) (ifHelper command state)) ; if declaration
      ((eq? (operator command) 'while) (whileHelper command state)) ; while declaration
      ((eq? (operator command) 'return) (returnHelper command state)) ; return declaration
      (else (error 'badoperation "Invalid command")) ; standard throw error
      )
    )
  )

(define operator
  (lambda (expression)
    (car (expression))))

(define operand1
  (lambda (expression)
    (cdr expression)))

(define operand2
  (lambda (expression)
    (cdr (cdr expression))))

(define valOrExpr ; either a value or an expression
  (lambda (expression)
    (car expression)))

; Function to evaluate integer val of expression
; Example input: (+ a b) and state ((x y ...) (1 2 ...))
; Returns error on any weirdness
(define MValue
        (lambda (expression state)
          (cond
            ((list? (valOrExpr expression))
             (MValue (valOrExpr expression) state))

           ; The operand expressions
           ((and (eq? (operator expression) '+) ; addition expression
                 (not (or (eq? (MValue (operand1 expression) state) "error")
                          (eq? (MValue (operand2 expression) state) "error"))))
           (+ (MValue (operand1 expression) state) (MValue (operand2 expression) state)))
           
           ((and (eq? (operator expression) '-) ; negation expression
                 (and (not (eq? (MValue (operand1 expression) state) "error"))
                      (null? (operand2 expression))))
           (- 0 (MValue (operand1 expression) state)))
           
           ((and (eq? (operator expression) '-) ; standard subtraction expression
                 (not (or (eq? (MValue (operand1 expression) state) "error")
                          (eq? (MValue (operand2 expression) state) "error"))))
           (- (MValue (operand1 expression) state) (MValue (operand2 expression) state)))
           
           ((and (eq? (operator expression) '*) ; multiplication expression
                 (not (or (eq? (MValue (operand1 expression) state) "error")
                          (eq? (MValue (operand2 expression) state) "error"))))
           (* (MValue (operand1 expression) state) (MValue (operand2 expression) state)))
           
           ((and (eq? (operator expression) '/) ; division expression
                 (not (or (eq? (MValue (operand1 expression) state) "error")
                          (eq? (MValue (operand2 expression) state) "error"))))
           (quotient (MValue (operand1 expression) state) (MValue (operand2 expression) state)))
           
           ((and (eq? (operator expression) '%) ; modulo expression
                 (not (or (eq? (MValue (operand1 expression) state) "error")
                          (eq? (MValue (operand2 expression) state) "error"))))
           (remainder (MValue (operand1 expression) state) (MValue (operand2 expression) state)))

           ((number? (valOrExpr expression)) (valOrExpr expression)) ; check if it's a normal number
           ((not (decCheck (valOrExpr expression) (varList state))) (lookup (valOrExpr expression) state)) ; check if it's a variable, if so, return its value
           (else "error")
            )
          )
        )

; Function to evaluate truth val of expression
; Example input: (== x y) and state ((x y ...) (1 2 ...))
(define MBool
        (lambda (condition state)
          (cond
            ((list? (valOrExpr condition))
             (MBool (valOrExpr condition) state))
            
            ((and (eq? (operator condition) '==) ; equality checker
                  (not (or (eq? (MValue (operand1 condition) state) "error")
                           (eq? (MValue (operand2 condition) state) "error"))))
             (eq? (MValue (operand1 condition) state) (MValue (operand2 condition) state)))
            
            ((and (eq? (operator condition) '!=) ; inequality checker
                  (not (or (eq? (MValue (operand1 condition) state) "error")
                           (eq? (MValue (operand2 condition) state) "error"))))
             (not(eq? (MValue (operand1 condition) state) (MValue (operand2 condition) state))))
            
            ((and (eq? (operator condition) '>) ; greater than checker
                  (not (or (eq? (MValue (operand1 condition) state) "error")
                           (eq? (MValue (operand2 condition) state) "error"))))
             (> (MValue (operand1 condition) state) (MValue (operand2 condition) state)))
            
            ((and (eq? (operator condition) '<) ; lesser than checker
                  (not (or (eq? (MValue (operand1 condition) state) "error")
                           (eq? (MValue (operand2 condition) state) "error"))))
             (< (MValue (operand1 condition) state) (MValue (operand2 condition) state)))
            
            ((and (eq? (operator condition) '>=) ; greater than/equal to checker
                  (not (or (eq? (MValue (operand1 condition) state) "error")
                           (eq? (MValue (operand2 condition) state) "error"))))
             (>= (MValue (operand1 condition) state) (MValue (operand2 condition) state)))
            
            ((and (eq? (operator condition) '<=) ; lesser than/equal to checker
                  (not (or (eq? (MValue (operand1 condition) state) "error")
                           (eq? (MValue (operand2 condition) state) "error"))))
             (<= (MValue (operand1 condition) state) (MValue (operand2 condition) state)))
            
            ((and (eq? (operator condition) '&&) ; and checker
                  (not (or (eq? (MBoolean (operand1 condition) state) "error")
                           (eq? (MBoolean (operand2 condition) state) "error"))))
             (and (MBoolean (operand1 condition) state) (MBoolean (operand2 condition) state)))
            
            ((and (eq? (operator condition) '||) ; or checker
                  (not (or (eq? (MBoolean (operand1 condition) state) "error")
                           (eq? (MBoolean (operand2 condition) state) "error"))))
             (or (MBoolean (operand1 condition) state) (MBoolean (operand2 condition) state)))
            
            ((and (eq? (operator condition) '!) ; not checker
                  (not (eq? (MBoolean (operand1 condition) state) "error")))
             (not (MBoolean (operand1 condition) state)))
            
            ((eq? (valOrExpr condition) 'true)  #t)
            ((eq? (valOrExpr condition) 'false)  #f)
            ((eq? (valOrExpr condition) '#t) #t)
            ((eq? (valOrExpr condition) '#f) #f)
            
            ((not (decCheck (valOrExpr condition) (varList state)))
             (if (or (eq? (lookup (valOrExpr condition) state) 'true)
                     (eq? (lookup (valOrExpr condition) state) '#t)) #t
                          (if (or (eq? (lookup (valOrExpr condition) state) 'false)
                              (eq? (lookup (valOrExpr condition) state) '#f)) #f "error")))
            )
          )
        )
                                     

;=====================;
;MState Helper Methods
;=====================;

; declare helper
; adds element to state if not declared
; Takes (var z) and state ((x y ...) (1 2 ...))
(define declareHelper
  (lambda (command state)
    (cond
      ((decCheck (commandVar command) (varList state))
       (addL command state)) ; returns state with new variable added if command valid
      (else "error") ; error otherwise
      )
    )
  )

(define commandVar
  (lambda (command)
    (car (cdr command))))

; assign helper
; Assigns value to an element in the state
; Takes (= z 4) and state ((x y ...) (1 2 ...))
(define assignHelper
  (lambda (command state)
    ((not (decCheck (commandVar command) (varList state))) ; returns state with var value if valid command
     (updateL command state))
    (else "error")
    )
  )


; if helper
; takes command (if bool true-expr false-expr) (false-expr optional) and state  ((x y) (1 2) ...))
(define ifHelper
  (lambda (command state)
    (cond
      ((MBool (ifCondition command) state) ; if the condition is true
       (MState (ifTrueExpr command) state)) ; returns state of program after true expression is run
      ((null? (ifFalseCheck command)) state) ; false expression is null so it will return the state
      (else (MState (ifFalseExpr command) state)) ; else run the false expression
      )
    )
  )

(define ifCondition
  (lambda (lis)
    (car (cdr lis))))

(define ifTrueExpr
  (lambda (lis)
    (car (cdr (cdr lis)))))

(define ifFalseCheck
  (lambda (lis)
    (cdr (cdr (cdr lis)))))

(define ifFalseExpr
  (lambda (lis)
    (car (cdr (cdr (cdr lis))))))

; while helper
; Runs for multiple iterations until condition is no longer true
; takes command (while bool body) and state ((x y ...) (1 2 ...))
(define whileHelper
  (lambda (command state)
    (cond
      ((null? (bodyCheck command))
       state)
      ((MBool (whileCondition command) state)
       (whileHelper command (MState (loopBody command) state)))
      (else state)
      )
    )
  )

(define bodyCheck
  (lambda (lis)
    (cdr (cdr lis))))

(define whileCondition
  (lambda (lis)
    (car (cdr lis))))

(define loopBody
  (lambda (lis)
    (car (cdr (cdr lis)))))

; return helper
; returns integer value of command if error not returned
; Takes (return (+ z 4)) and state ((x y ...) (1 2 ...))
(define returnHelper
  (lambda (command state)
    (cond
      ((not (eq? (MValue (returnExpression command) state) "error"))
       (MValue (returnExpression command) state)) ; Returns int value of command if MValue doesn't return "error"
      (else (MBool (returnExpression command) state)) ; Returns the boolean value of the command if it wasn't a number value
      )
    )
  )

(define returnExpression
  (lambda (lis)
    (car (cdr (lis)))))


;======================;
;Other Helper Methods
;======================;
; returns #t if the variable being assigned has not been declared
; takes assignment command (= z 6) and state-vars (x y z ...) aka (car state)
(define decCheck
  (lambda (var state-vars)
    (cond
      ((null? state-vars)
       #t) ; null state
      ((eq? (car state-vars) var)
       #f) ; returns #f if variable is declared
      (else (decCheck var (cdr state-vars)))
      )
    )
  )

; Adds the variable and value
; takes command (var x expr) (expr optional) and state ((z y ...) (1 2 ...))
(define addL
  (lambda (command state)
    (cond
      ((null? (commandExpression command)) ; to check if expressions has been passed
       (cons (cons (commandVar command) (varList state))
             (cons (cons "undefined" (car (valList state))) '())))
      
      ((eq? (MValue (commandExpression command) state) "error") ; if the state is an error from MValue, it must have been declared with boolean value
       (cons (cons (commandVar command) (varList state))
             (cons (cons (MBool (commandExpression command) state)
                         (car (valList state))) '())))
      
      (else (cons (cons (commandVar command) (varList state)) (cons (cons (MValue (commandExpression command) state) (car (valList state))) '())))
      )
    )
  )

(define commandExpression
  (lambda (lis)
    (car (cdr (cdr lis)))))

(define varList
  (lambda (state)
    (car state)))

(define valList
  (lambda (state)
    (cdr state)))

; updates the state with the variable-value pair
; takes command (= x expr) and state ((x y ...) (1 2 ...))
(define updateL
  (lambda (command state)
    (cond
      ((null? (varList state))
       #f) ; Should never get here because we already checked to see if variable can be added to list
      ((and (eq? (commandVar command) (car (varList state))) (eq? (MValue (commandExpression command) state) "error")) ; expression is a boolean
       (cons (varList state) (cons (cons (MBool (commandExpression command) state) (otherVals state)) '())))
      ((eq? (commandVar command) (car (varList state))) ; expression is an integer value
       (cons (varList state) (cons (cons (MValue (commandExpression command) state) (otherVals state)) '())))
      (else (cons (cons (firstVar state) (varList (updateL command (cons (otherVars state) (cons (otherVals state) '())))))
                  (cons (cons (firstVal state)
                              (valList (updateL command (cons (otherVars state) (cons (otherVals state) '()))))) '())))
    )
  )
  )

(define otherVars
  (lambda (state)
    (cdr (car state))))

(define otherVals
  (lambda (state)
    (cdr (car (cdr state)))))

(define firstVar
  (lambda (state)
    (car (car state))))

(define firstVal
  (lambda (state)
    (car (car (cdr state)))))
  
; Returns the value of the variable as stored in the state
; takes var as a variable name and state ((x y ...) (1 2 ...))
(define lookup
  (lambda (var state)
    (cond
      ((null? (varList state))
       "error") ; returns error if x isn't in the state
      ((eq? (firstVar state) var)
       (firstVal state)) ; returns the first value if the first variable is equal to var
      (else (lookup var (cons (otherVars state) (cons (otherVals state) '()))))
      )
    )
  )