; Interpreter Pt 2 EECS 345
; Group Members: Lee Radics, Zach Perlo, Rohan Krishna
; Case IDs: elr61, zip5, rxr353
; Loading in the simpleParser file to allow for use of the parser.
(load "simpleParser.scm")

; The interpreter function used to parse/interpret the javaish file (.javaish)
(define interpret
  (lambda (filename)
    (call/cc
      (lambda (return)
        (let*
         ((initReturn (lambda (statement env) 
          (return (MValue (getArg statement 2) 
            env return dBreak dContinue dThrow))))
          (outEnv (dInterpret (parser filename) '(((true false) (true false))) 
            (lambda (statement env) (return env)) dBreak dContinue dThrow)))))
      )
    )
  )

; Recursively evaluate each statement in the program
(define dInterpret
  (lambda (statement state return break continue throw)
    (if (or (not (list? state)) (null? statement)) state
      (dInterpret (cdr statement)
                    (MState (operator statement) state return break continue throw)
                    return break continue throw)
      )
    )
  )

; Returns the state of the program after running command
(define MState
  (lambda (statement state return break continue throw)
    (cond
      ((not (list? state)) state)
      ; Assign Declaration
      ((eq? (operator statement) '=) 
        (assignHelper statement state return break continue throw)) 
      ; Begin declaration
      ((eq? (operator statement) 'begin) 
        (beginHelper (operand1 statement) state return break continue throw))
      ; Break declaration
      ((eq? (operator statement) 'break) 
        (break state))
      ; Continue declaration
      ((eq? (operator statement) 'continue) 
        (continue state))
      ; If declaration
      ((eq? (operator statement) 'if) 
        (ifHelper statement state return break continue throw))
      ; Return declaration
      ((eq? (operator statement) 'return) 
        (return statement state))
      ; Throw declaration
      ((eq? (operator statement) 'throw) 
        (throw (MValue (commandVar statement) state return break continue throw)))
      ; Try declaration
      ((eq? (operator statement) 'try) 
        (tryCatchFinallyHelper statement state return break continue throw))
      ; Variable declaration
      ((eq? (operator statement) 'var) 
        (declareHelper statement state return break continue throw))
      ; While Helper
      ((eq? (operator statement) 'while)
        (call/cc
          (lambda (new-break)
            (whileHelper (getArg statement 2) (getArg statement 3) state return new-break continue throw))))
      ; Otherwise error
      (else (throwerror 'Error:InvalidCommand))
      )
    )
  )
;=====================;
;MState Helper Methods;
;=====================;

; Assign helper
; Assigns value to an element in the state
; Takes (= z 4) and state ((x y ...) (1 2 ...))
(define assignHelper
  (lambda (statement env r b c t)
    (replaceVar (commandVar statement) (MValue (commandExpression statement) env r b c t) env)
  ))

; Runs each statement inside a begin block after adding a layer to the state
(define beginHelper
  (lambda (statement env return break continue throw)
    (otherLayers (dInterpret statement (addLevelOfScope env) 
      return (lambda (s) (break (otherLayers s))) (lambda (s) (continue (otherLayers s))) throw))
  ))

; If helper
; Takes command (if bool true-expr false-expr) (false-expr optional) and state  ((x y) (1 2) ...))
(define ifHelper
  (lambda (statement env return break continue throw)
    (cond
      ((eq? 'true (MBool (getArg statement 2) env return break continue throw)) 
        (MState (getArg statement 3) env return break continue throw))
      ((not (null? (getArg statement 4))) (MState (getArg statement 4) 
        env return break continue throw))
      (else env)
      )
    )
  )

; Handle try/catch/finally blocks
(define tryCatchFinallyHelper
  (lambda (statement env return break continue throw)
    (call/cc
     (lambda (catch-continuation)
       (letrec
           ((finally (lambda (s)
            (if (pair? (getArg statement 4))
              (beginHelper (getFinallyBody statement) s return break continue throw) s)))
            (try (lambda (newThrow)
              (if (pair? (getArg statement 3))
                (finally (beginHelper (getArg statement 2) env return break continue newThrow))
                (finally (beginHelper (getArg statement 2) env return break continue throw)))))
            (catch (lambda (e s)
              (finally (beginCatch (getCatchBody statement) 
                (getCatchErr statement) e s return break continue throw)))))
         (try (lambda (e) (catch-continuation (catch e env))))
         )
       )
     )
    )
  )

; Same as beginHelper, but for try/catch/finally blocks
(define beginCatch
  (lambda (statement eName eValue env return break continue throw)
    (operand1
     (dInterpret
      statement
      (insert eName eValue (addLevelOfScope env))
      return
      (lambda (s) (break (operand1 s)))
      (lambda (s) (continue (operand1 s)))
      throw)
     )
    )
  )

; Declare helper
; Adds element to state if not declared
; Takes (var z) and state ((x y ...) (1 2 ...))
(define declareHelper
  (lambda (statement env r b c t)
    (cond
      ((null? (getArg statement 3)) 
        (insert (getArg statement 2) 'undefined env))
      (else (insert (getArg statement 2) 
        (MValue (getArg statement 3) env r b c t) env))
      )
    )
  )

; While helper
; Runs for multiple iterations until condition is no longer true
; Takes command (while bool body) and state ((x y ...) (1 2 ...))
(define whileHelper
  (lambda (condition statement env return break continue throw)
    (if (eq? 'true (MBool condition env return break continue throw))
      (whileHelper
       condition
       statement
       (call/cc (lambda (newCont) (MState statement env return break newCont throw)))
       return
       break
       continue
       throw)
      env)
    )
  )

;========================;
;Language Helper Methods;
;========================;
(define operator (lambda (expression) (car expression)))

(define operand1 (lambda (expression) (cdr expression)))

(define operand2 (lambda (expression) (cddr expression)))

(define valOrExpr (lambda (expression) (car expression))) ; either a value or an expression

(define blockBody (lambda (blk) (cdr blk)))

;==MValue==;
; Evaluate the integer value of an expression
; Given expression (+ a b) and state ((x y ...) (1 2 ...))
; Returns "error" on any abnormalities
(define MValue
  (lambda (statement env r b c t)
    (cond
      ((number? statement) statement)

      ((eq? statement 'true) 'true)

      ((eq? statement 'false) 'false)

      ((not (list? statement)) (lookup statement env))

      ; Addition helper
      ((eq? (operator statement) '+) 
        (+ (MValue (getArg statement 2) env r b c t) 
          (MValue (getArg statement 3) env r b c t)))

      ; Subtraction helper
      ((eq? (operator statement) '-) (if (null? (operand2 statement)) 
      (- (MValue (getArg statement 2) env r b c t)) ; Unary "-"
      (- (MValue (getArg statement 2) env r b c t) (MValue (getArg statement 3) env r b c t))))

      ; Multiplication helper
      ((eq? (operator statement) '*) 
        (* (MValue (getArg statement 2) env r b c t) 
          (MValue (getArg statement 3) env r b c t)))

      ; Division helper
      ((eq? (operator statement) '/) 
        (quotient (MValue (getArg statement 2) env r b c t) 
          (MValue (getArg statement 3) env r b c t)))

      ; Modulus helper
      ((eq? (operator statement) '%) 
        (remainder (MValue (getArg statement 2) env r b c t) 
          (MValue (getArg statement 3) env r b c t)))

      (else (MBool statement env r b c t))
      )
    )
  )


;==MBool==;
; Function to evaluate truth val of expression
; Example input: (== x y) and state ((x y ...) (1 2 ...))
; Returns 'true or 'false if the expression is valid
(define MBool
  (lambda (statement state r b c t)
    (cond
      ((not (list? statement)) (MValue statement state r b c t))
      ; True
      ((eq? statement 'true) 'true)
      ; False
      ((eq? statement 'false) 'false)

      ((not (list? statement)) (MValue statement state r b c t))

      ; Greater than checker
      ((eq? (operator statement) '>) 
        (if (> (MValue (getArg statement 2) state r b c t) 
          (MValue (getArg statement 3) state r b c t)) 'true 'false))

      ; Lesser than checker
      ((eq? (operator statement) '<) 
        (if (< (MValue (getArg statement 2) state r b c t) 
          (MValue (getArg statement 3) state r b c t)) 'true 'false))

      ; Greater than or equal to checker
      ((eq? (operator statement) '>=) 
        (if (>= (MValue (getArg statement 2) state r b c t) 
          (MValue (getArg statement 3) state r b c t)) 'true 'false))

      ; Lesser than or qual to checker
      ((eq? (operator statement) '<=) 
        (if (<= (MValue (getArg statement 2) state r b c t) 
          (MValue (getArg statement 3) state r b c t)) 'true 'false))

      ; Equality checker
      ((eq? (operator statement) '==) 
        (if (= (MValue (getArg statement 2) state r b c t) 
          (MValue (getArg statement 3) state r b c t)) 'true 'false))

      ; Inequality checker
      ((eq? (operator statement) '!=) 
        (if (not (= (MValue (getArg statement 2) state r b c t) 
          (MValue (getArg statement 3) state r b c t))) 'true 'false))

      ; And Checker
      ((eq? (operator statement) '&&) 
        (if (eq? #t (and (eq? 'true (MBool (getArg statement 2) state r b c t)) 
          (eq? 'true (MBool (getArg statement 3) state r b c t)))) 'true 'false))

      ; Or checker
      ((eq? (operator statement) '||) 
        (if (eq? #t (or (eq? 'true (MBool (getArg statement 2) state r b c t)) 
          (eq? 'true (MBool (getArg statement 3) state r b c t)))) 'true 'false))

      ; Not checker
      ((eq? (operator statement) '!) 
        (if (eq? #t (not (eq? 'true (MBool (getArg statement 2) state r b c t)))) 'true 'false))
      
      (else (throwerror 'Error:InvalidBooleanExpression))
      )
    )
  )

;====================;
;Other Helper Methods;
;====================;

; Returns the value of the variable as stored in the state
; Takes var as a variable name and state {((a b) (3 4 ...)) ((x y ...) (1 2 ...))}
(define lookup
  (lambda (var state)
    (cond
      ((null? state) (throwerror 'Error:UndeclaredVariable))
      ((inEnvironment? var (topLayerVars state)) (lookupVal var (topLayer state)))
      (else (lookup var (otherLayers state)))
      )
    )
  )

; Helper for lookup
(define lookupVal
  (lambda (var state)
    (cond
      ((eq? (firstVar state) var) (unbox (firstVal state)))
      (else (lookupVal var (cons (otherVars state) (cons (otherVals state) '()))))
      )
    )
  )

; Update variable value
(define replaceVar
  (lambda (var value state)
    (cond
      ((null? state) (throwerror 'Error:UndeclaredVariable))
      ((inEnvironment? var 
        (caar state)) (cons (getReplaced var value (topLayer state)) (otherLayers state)))
      (else (cons (topLayer state) 
        (replaceVar var value (otherLayers state))))
      )
    )
  )

; Helper for replaceVar
(define getReplaced
  (lambda (var value state)
    (cond
      ((eq? (firstVar state) var) (cons (cons var (otherVars state)) 
        (cons (cons (begin (set-box! 
          (firstVal state) value) (firstVal state)) 
          (otherVals state)) '())))
      (else (car (insert (firstVar state) (unbox (firstVal state)) 
        (cons (getReplaced var value (cons (otherVars state) 
          (cons (otherVals state) '()))) '()))))
      )
    )
  )

; Add variable to state
(define insert
  (lambda (var value state)
    (cons (cons (cons var (firstVar state)) 
      (cons (cons (box value) (topLayerFirstVal state)) '())) (valList state))
    )
  )

; Check if the variable is in the given environment
(define inEnvironment?
  (lambda (var vList)
    (cond
     ((null? vList) #f)
     ((eq? var (topLayer vList)) #t)
     (else (inEnvironment? var (resOfVariablesInState vList))))
    )
  )

(define resOfVariablesInState cdr)

; Adds a level of scope to the given state
(define addLevelOfScope
  (lambda (state)
    (cons '(()()) state)))

;=====Abstraction Methods=====;

(define throwerror
  (lambda (err) (error err)
    )
  )

(define dBreak
  (lambda (s)
    (throwerror 'Error:InvalidBreak)
    )
  )

(define dContinue
  (lambda (s)
    (throwerror 'Error:InvalidContinue)
    )
  )

(define dThrow
  (lambda (e s)
    (throwerror 'Error:UncaughtValue)
    )
  )

(define getArg
  (lambda (l i)
    (cond
      ((null? l) l)
      ((eq? i 1) (topLayer l))
      (else (getArg (otherLayers l) (- i 1)))
      )
    )
  )

;==========================;
;Declaration Helper Methods;
;==========================;
(define getCatchBody (lambda (v) (caddr (caddr v))))

(define getCatchErr (lambda (v) (caadr (caddr  v))))

(define getFinallyBody (lambda (t) (cadar (cdddr t))))

(define commandExpression (lambda (lis) (caddr lis)))

(define varList (lambda (state) (car state)))

(define valList (lambda (state) (cdr state)))

(define commandVar (lambda (command) (cadr command)))

(define otherVars (lambda (state) (cdar state)))

(define otherVals (lambda (state) (cdadr state)))

(define firstVar (lambda (state) (caar state)))

(define firstVal (lambda (state) (caadr state)))

(define otherCommand (lambda (state) (cddr state)))

(define topLayer (lambda (state) (car state)))

(define topLayerVars (lambda (state) (caar state)))

(define topLayerVals (lambda (state) (cdar state)))

(define otherLayers (lambda (state) (cdr state)))

(define topLayerFirstVar (lambda (state) (caaar state)))

(define topLayerFirstVal (lambda (state) (cadar state)))

(define topLayerOtherVals (lambda (state) (cddar state)))