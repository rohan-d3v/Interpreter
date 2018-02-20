; you must set lang to "Pretty Big"
(load "simpleParser.scm")
(load "interpret.scm")

(define testcases
 '(("tests/test1.javaish" 150)
   ("tests/test2.javaish" -4)
   ("tests/test3.javaish" 10)
   ("tests/test4.javaish" 16)
   ("tests/test5.javaish" 220)
   ("tests/test6.javaish" 5)
   ("tests/test7.javaish" 6)
   ("tests/test8.javaish" 10)
   ("tests/test9.javaish" 5)
   ("tests/test10.javaish" -39)
  ; ("tests/test11.javaish" "error")
  ; ("tests/test12.javaish" "error")
  ; ("tests/test13.javaish" "error")
  ; ("tests/test14.javaish" "error")
   ("tests/test15.javaish" " error")
   ("tests/test16.javaish" 100)
   ("tests/test17.javaish" "false")
   ("tests/test18.javaish" "true")
   ("tests/test19.javaish" 128)
   ("tests/test20.javaish" 12)
   ; advanced tests
   ;("tests/test21.javaish" 30)
   ;("tests/test22.javaish" 11)
   ;("tests/test23.javaish" 1106)
   ;("tests/test24.javaish" 12)
   ;("tests/test25.javaish" 16)
   ;("tests/test26.javaish" 72)
   ;("tests/test27.javaish" 21)
   ;("tests/test28.javaish" 164)
   )
  )
(define test-first
  (lambda (testcases)
    (begin
      (display (caar testcases)) (display ": ")
      (display (eq? (interpret (caar testcases)) (cadar testcases)))
      (display "  =") (display (interpret (caar testcases)))
      (display "  goal= ") (display (cadar testcases)) (newline)
      )))
(define do-tests
  (lambda (testcases)
    (cond
      ((null? testcases) (display "We passed!"))
      ((null? (car testcases)) "tests")
      (else
        (test-first testcases)
        (do-tests (cdr testcases)))
      )))
(do-tests testcases)