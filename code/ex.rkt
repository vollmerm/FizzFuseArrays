#lang racket

(require "pull.rkt")
(require "push.rkt")
(require "spull.rkt")
(require "fizz.rkt")

(define scalar-product
  (lambda (arr1 arr2)
    (fold + 0 (zipwith * arr1 arr2))))

(define testarr1 (vector->pull #(1 2 3 4 5)))
(define testarr2 (vector->pull #(5 4 3 2 1)))

(scalar-product testarr1 testarr2) ;=> 35

(define staged-scalar-product
  (lambda (arr1 arr2)
    (sfold s+ 0 (szipwith s* arr1 arr2))))

(define testarr3 `(pull ,(lambda (i) `(f ,i)) 5))
(define testarr4 `(pull ,(lambda (i) `(g ,i)) 5))

(staged-scalar-product testarr3 testarr4)
;; '(let loop8413 ((acc8411 0) (i8412 0))
;;    (if (= i8412 5) acc8411
;;        (loop8413 (+ acc8411 (* (f i8412) (g i8412))) (add1 i8412))))


(fission-compute (fission-map add1 testarr1)) ;=> '#(2 3 4 5 6)