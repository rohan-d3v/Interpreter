#lang racket
; Interpreter Pt 1 EECS 345
; Group Members: Lee Radics, Zach Perlo, Rohan Krishna
; Case IDs:elr61, zip5, rxr353

(load "simpleParser.scm") ;Load simple parser to enable parser use

;The interpreter function used to parse/interpret the java-like file (.javal)
(define interpret
  (lambda (filename)
    (evaluate (parser filename) '(()()));passes the filename to evaluate/parse program tree
    )
  )
  

;Function to evaluate and step through program tree
(define evaluate
  (lambda (prgm state)
    (cond
      ((and (null? prgm) (eq? state #t)) "true");if prgm null and equals state true
      ((and (null? prgm) (eq? state #f)) "false")
      ((null? prgm) state);now finally check if prgm is null, then return state
      (else (evaluate (cdr prgm) (MState (car prgm) state)));evaluate filename while calling MState on the command
      )                                                     ;if checks are passed 
    )
  )

;Function to return program state after command is run
;Commands: var, =, if, while, return and the state is ((x y ...) (1 2 ...))
(define MState
  (lambda (command state)
    (cond
      ((eq? state "error") "error")
      ((eq? (car command) 'var) (declareHelper command state))
      ((eq? (car command) '=) (assignHelper command state))
      ((eq? (car command) 'if) (ifHelper command state))
      ((eq? (car command) 'while) (whileHelper command state))
      ((eq? (car command) 'return) (returnHelper command state))
      (else "error")
      )
    )
  )