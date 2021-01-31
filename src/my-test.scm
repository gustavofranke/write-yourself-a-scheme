(define test1 (+ 2 2))
(define test2 (+ 2 (- 4 1)))
(define test3 (- (+ 4 6 3) 3 5 2))
(define test4 (< 2 3))
(define test5 (> 2 3))

(define test6 (>= 2 3))
(define test7 (>= 3 3))
(define test8 (string=? "test" "test"))
(define test9 (string<=? "abc" "bba"))
(define test10 (if (> 2 3) "no" "yes"))
(define test11 (if (= 3 3) (+ 2 3 (- 5 1)) "unequal"))

(define test12 (car '(2 3 4)))
(define test13 (cdr '(2 3 4)))
(define test14 (car (cdr (cons 2 '(3 4)))))
(define test15 (cdr '(a simple test)))
(define test16 (car (cdr '(a simple test))))
(define test17 (car '((this is) a test)))
(define test18 (cons '(this is) 'test))
(define test19 (cons '(this is) '()))

(define test20 (eqv? 1 3))
(define test20 (eqv? 3 3))
(define test21 (eqv? 'atom 'atom))
(define test22 (+ 2 3))
(define test23 (cons 'this '()))
(define test24 (cons 2 3))

(define x 3)
(define test25 (+ x 2))
(define y 5)
(define test26 (+ x (- y 2)))
(define str "A string")

(define (f x y) (+ x y))
(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))
(define (counter inc) (lambda (x) (set! inc (+ x inc))))
(define my-count (counter 5))

(define test27 (f 1 2))

(define test28 (factorial 10))

(define test29 (my-count 3))

(define test30 (my-count 6))

(define test31 (my-count 5))

(load "src/stdlib.scm")

(define test32 (sum 1 2 3 4))
(define test33 (product 1 2 3 4))
(define test34 (and #t #t #f))
(define test35 (or #t #t #f))
(define test36 (max 1 2 9 9))
(define test37 (max 1 2 9 1 2 9))
(define test38 (min 1 2 9 1 2 9))

(define test39 (length '()))
(define test40 (length '(1 2 3)))
(define test41 (reverse '(1 2 3)))
(define test42 (filter even? '(1 2 3 4)))
(define test43 (map even? '(1 2 3 4)))
