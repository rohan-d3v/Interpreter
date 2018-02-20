; Programing Project Part #1 EECS 345
; Group Members: Justin Lee(jpl88), Ian Waldschmidt(isw5), Aaron Cutright(ahc49)

; Loading in the simpleParser file to allow for use of the parser.
(load "simpleParser.scm")

; The interpreter function used to parse/interpret the javaish file (.javaish)
(define interpret
  (lambda (filename)
    (evaluate (parser filename) initState) ;Passes the filename to evaluate/parse program tree
    ))

(define initState '(()())) ;Starting value of the state

; Step through each line in the program tree
(define evaluate
  (lambda (prgm state)
    (cond
      ((and (null? prgm) (eq? state #t)) "true") ; if return was called with the value of true
      ((and (null? prgm) (eq? state #f)) "false") ; if return was called with the value of false
      ((null? prgm) state) ;now finally check if prgm is null, then return state
      (else (evaluate (cdr prgm) (MState (car prgm) state)))  ;evaluate filename while calling MState on the command
      ) ; if checks are passed 
    )
  )

;==MState==;
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
      (else "error") ; standard throw error
      )
    )
  )

;==MValue==;
; Evaluate the integer value of an expression
; given expression (+ a b) and state ((x y ...) (1 2 ...))
; returns "error" on any abnormalities
(define MValue
 (lambda (expression state)
   (cond
     ((list? (valOrExpr expression)) 
      (MValue (valOrExpr expression) state))

    ; Operand Expressions
     ((and (eq? (operator expression) '+) ;Addition expression
      (not (or (eq? (MValue (operand1 expression) state) "error") 
        (eq? (MValue (operand2 expression) state) "error"))))
     (+ (MValue (operand1 expression) state) (MValue (operand2 expression) state)))

     ((and (eq? (operator expression) '-) ;Negation expression
      (and (not (eq? (MValue (operand1 expression) state) "error")) 
        (null? (operand2 expression))))
     (- 0 (MValue (operand1 expression) state)))

     ((and (eq? (operator expression) '-) ;Subtraction expression
      (not (or (eq? (MValue (operand1 expression) state) "error") 
        (eq? (MValue (operand2 expression) state) "error"))))
           (- (MValue (operand1 expression) state) (MValue (operand2 expression) state)))

     ((and (eq? (operator expression) '*) ;Multiplication expression
      (not (or (eq? (MValue (operand1 expression) state) "error") 
        (eq? (MValue (operand2 expression) state) "error"))))
           (* (MValue (operand1 expression) state) (MValue (operand2 expression) state)))

     ((and (eq? (operator expression) '/) ;Division expression
      (not (or (eq? (MValue (operand1 expression) state) "error") 
        (eq? (MValue (operand2 expression) state) "error"))))
     (floor (/ (MValue (operand1 expression) state) (MValue (operand2 expression) state))))

     ((and (eq? (operator expression) '%) ;Modulo expression
      (not (or (eq? (MValue (operand1 expression) state) "error") 
        (eq? (MValue (operand2 expression) state) "error"))))
     (remainder (MValue (operand1 expression) state) (MValue (operand2 expression) state)))

     ((number? (valOrExpr expression)) (valOrExpr expression)) ;Check if normal number
     ((lCheck (valOrExpr expression) state) (lookup (valOrExpr expression) state))
     (else "error")
     )
   )
 )

;==MBool==;
; Function to evaluate truth val of expression
; Example input: (== x y) and state ((x y ...) (1 2 ...))
(define MBool
 (lambda (condition state)
   (cond
     ((list? (valOrExpr condition))
      (MBool (valOrExpr condition) state))

     ((and (eq? (operator condition) '==) ;Equality checker
      (not (or (eq? (MValue (operand1 condition) state) "error")
        (eq? (MValue (operand2 condition) state) "error"))))
      (eq? (MValue (operand1 condition) state) (MValue (operand2 condition) state)))

     ((and (eq? (operator condition) '!=) ;Inequality checker
      (not (or (eq? (MValue (operand1 condition) state) "error")
        (eq? (MValue (operand2 condition) state) "error"))))
      (not(eq? (MValue (operand1 condition) state) (MValue (operand2 condition) state))))

     ((and (eq? (operator condition) '>) ;Greater than checker
      (not (or (eq? (MValue (operand1 condition) state) "error")
        (eq? (MValue (operand2 condition) state) "error"))))
      (> (MValue (operand1 condition) state) (MValue (operand2 condition) state)))

     ((and (eq? (operator condition) '<) ;Less than checker
      (not (or (eq? (MValue (operand1 condition) state) "error")
        (eq? (MValue (operand2 condition) state) "error"))))
      (< (MValue (operand1 condition) state) (MValue (operand2 condition) state)))

     ((and (eq? (operator condition) '>=) ;Greater than equal to checker
      (not (or (eq? (MValue (operand1 condition) state) "error")
        (eq? (MValue (operand2 condition) state) "error"))))
      (>= (MValue (operand1 condition) state) (MValue (operand2 condition) state)))

     ((and (eq? (operator condition) '<=) ;Lesser than/ Equal to checker
      (not (or (eq? (MValue (operand1 condition) state) "error")
        (eq? (MValue (operand2 condition) state) "error"))))
      (<= (MValue (operand1 condition) state) (MValue (operand2 condition) state)))

     ((and (eq? (operator condition) '&&) ;And checker
      (not (or (eq? (MBool (operand1 condition) state) "error")
        (eq? (MBool (operand2 condition) state) "error"))))
      (and (MBool (operand1 condition) state) (MBool (operand2 condition) state)))

     ((and (eq? (operator condition) '||) ;Or checker
      (not (or (eq? (MBool (operand1 condition) state) "error")
        (eq? (MBool (operand2 condition) state) "error"))))
      (or (MBool (operand1 condition) state) (MBool (operand2 condition) state)))

     ((and (eq? (operator condition) '!) ;Not checker
      (not (eq? (MBool (operand1 condition) state) "error")))
     (not (MBool (operand1 condition) state)))

     ((eq? (valOrExpr condition) 'true)  #t)
     ((eq? (valOrExpr condition) 'false)  #f)
     ((eq? (valOrExpr condition) '#t) #t)
     ((eq? (valOrExpr condition) '#f) #f)

     ((lCheck (valOrExpr condition) state) 
      (if (or (eq? (lookup (valOrExpr condition) state) 'true)
        (eq? (lookup (valOrExpr condition) state) '#t)) #t
      (if (or (eq? (lookup (valOrExpr condition) state) 'false)
        (eq? (lookup (valOrExpr condition) state) '#f)) #f "error")))

     (else "error")
     )
   )
 )

;========================;
;Language Helper Methods;
;========================;
(define operator (lambda (expression) (car expression)))

(define operand1 (lambda (expression) (cdr expression)))

(define operand2 (lambda (expression) (cddr expression)))

; either a value or an expression
(define valOrExpr (lambda (expression) (car expression)))

;=====================;
;MState Helper Methods;
;=====================;

; Declare helper
; adds element to state if not declared
; Takes (var z) and state ((x y ...) (1 2 ...))
(define declareHelper
  (lambda (command state)
    (cond
      ((decCheck command (commandVar state)) 
        (addL command state)) ;returns state with new variable added if command valid
      (else "error") ;error otherwise
      )
    )
  )

; Assign helper
; Assigns value to an element in the state
; Takes (= z 4) and state ((x y ...) (1 2 ...))
(define assignHelper
  (lambda (command state)
    (cond
      ((lCheck (commandVar command) state)
       (cons (varList state) (cons (updateL (valList command) state)
                               '()))) ;Returns state with updated val if valid command
      (else "error")
      )
    )
  )

; If helper
; takes command (if bool true-expr false-expr) (false-expr optional) and state  ((x y) (1 2) ...))
(define ifHelper
  (lambda (command state)
    (cond
      ((MBool (ifCondition command) state) ;If comdition trye
       (MState (ifTrueExpr command) state)) ;Retusn state after true expression run
      ((null? (ifFalseCheck command)) state) ;False expression null so will return state
      (else (MState (ifFalseExpr command) state)) ;Else runs false expression
      )
    )
  )

(define ifCondition (lambda (lis)(cadr lis)))

(define ifTrueExpr (lambda (lis) (caddr lis)))

(define ifFalseCheck (lambda (lis) (cdddr lis)))

(define ifFalseExpr (lambda (lis) (cadddr lis)))

; While helper
; Runs for multiple iterations until condition is no longer true
; takes command (while bool body) and state ((x y ...) (1 2 ...))
(define whileHelper
  (lambda (command state)
    (cond
      ((MBool (whileCondition command) state) (MState command (MState (caddr command) state)))
      (else state)
      )
    )
  )


(define whileCondition (lambda (lis) (cadr lis)))

(define loopBody (lambda (lis) (caddr lis)))

; Return helper
; returns integer value of command if error not returned
; Takes (return (+ z 4)) and state ((x y ...) (1 2 ...))
(define returnHelper
  (lambda (command state)
    (cond
      ((not (eq? (MValue (returnExpression command) state) "error"))
       (MValue (returnExpression command) state)) ;Returns int value of command if Mvalue doesn't return error
      (else (MBool (returnExpression command) state)) ;Returns boolean value if command was not a number
      )
    )
  )

(define returnExpression (lambda (lis) (cdr lis)))

;==========================;
;Declaration Helper Methods;
;==========================;

(define commandExpression (lambda (lis) (caddr lis)))

(define varList (lambda (state) (car state)))

(define valList (lambda (state) (cdr state)))

(define commandVar (lambda (command) (cadr command)))

(define otherVars (lambda (state) (cdar state)))

(define otherVals (lambda (state) (cdadr state)))

(define firstVar (lambda (state) (caar state)))

(define firstVal (lambda (state) (caadr state)))

(define otherCommand (lambda (state) (cddr state)))

;====================;
;Other Helper Methods;
;====================;

; returns #t if the variable being assigned has not been declared
; takes assignment command (= z 6) and state-vars (x y z ...) aka (car state)
(define decCheck
  (lambda (command state-vars)
    (cond
      ((null? state-vars) #t) ;Null state
      ((eq? (varList state-vars) (commandVar command)) #f) ;f if valriable declared
      (else (decCheck command (valList state-vars)))
      )
    )
  )

; Adds the variable and value
; takes command (var x expr) (expr optional) and state ((z y ...) (1 2 ...))
(define addL
  (lambda (command state)
    (cond
      ((null? (otherCommand command)) ;to check if expressions has been passed
       (cons (cons (commandVar command) (varList state))
             (cons (cons "undefined" (commandVar state)) '())))
      
      ((eq? (MValue (cons (commandExpression command) '()) state) "error") ;if the state is an error from MValue, it must have been declared with boolean value
       (cons (cons (commandVar command) (varList state))
             (cons (cons (MBool (cons (commandExpression command) '()) state)
                         (commandVar state)) '())))
      
      (else (cons (cons (commandVar command) (varList state))
                  (cons (cons (MValue (cons (commandExpression command) '()) state)
                              (commandVar state)) '())))
      )
    )
  )

; returns #t if the variable has been declared (is in the state)
; takes var 'a and state ((x y ...) (1 2 ...))
(define lCheck
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((null? (varList state)) #f)
      ((eq? var (firstVar state)) #t)
      (else (lCheck var (cons (otherVars state) (cons (otherVals state) '()))))
      )
    )
  )

; updates the state with the variable-value pair
; takes command (= x expr) and state ((x y ...) (1 2 ...))
(define updateL
  (lambda (command state)
    (cond
      ((null? (varList state)) #f) ; Should never get here because we already checked to see if variable can be added to list
      ((eq? (varList command) (firstVar state))
       (cons (MValue (cons (commandVar command) '()) state)
             (otherVals state))) ; expression is a boolean
      (else (cons (firstVal state)
                  (updateL command (cons (otherVars state) (cons (otherVals state)
                                                            '())))))
      )
    )
  )

; Returns the value of the variable as stored in the state
; takes var as a variable name and state ((x y ...) (1 2 ...))
(define lookup
  (lambda (var state)
    (cond
      ((null? (varList state)) "error") ;returns error if X isn't in state
      ((eq? (firstVar state) var)
       (firstVal state)); returns the first value if the first variable is equal to var
      (else (lookup var (cons (otherVars state) (cons (otherVals state) '()))))
      )
    )
  )