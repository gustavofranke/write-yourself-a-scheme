(define (f x y) (+ x y))
(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))
(define (counter inc) (lambda (x) (set! inc (+ x inc))))
(define my-count (counter 5))

(define test1 (f 1 2))

(define test2 (factorial 10))

(define test3 (my-count 3))

(define test4 (my-count 6))

(define test5 (my-count 5))
