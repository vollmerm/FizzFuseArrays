#lang racket

(provide (all-defined-out))

;; pull arrays

(define pull
  (lambda (f len) 
    `(pull ,f ,len)))

(define pull->vector
  (lambda (arr)
    (match arr
      [`(pull ,f ,len)
       (build-vector len f)])))

(define vector->pull
  (lambda (vect) 
    (let ([f (lambda (i) (vector-ref vect i))]
          [len (vector-length vect)])
      (pull f len))))

(define map
  (lambda (f arr) 
    (match arr
      [`(pull ,g ,len)
       (let ([h (lambda (i) (f (g i)))])
         (pull h len))])))

(define zipwith
  (lambda (f arr1 arr2)
    (match-let
        ([`(pull ,f1 ,len1) arr1]
         [`(pull ,f2 ,len2) arr2])
      (let ([g (lambda (i) (f (f1 i) (f2 i)))]
            [len (min len1 len2)])
        (pull g len)))))

(define fold
  (lambda (f init arr)
    (match arr
      [`(pull ,g ,len)
        (let loop ([acc init] [i 0])
          (if (= i len) acc
              (loop (f acc (g i)) (add1 i))))])))

(define compute
  (lambda (arr)
    (match arr
      [`(pull ,f ,len)
       (let ([vec (pull->vector arr)])
         `(pull ,(lambda (i) (vector-ref vec i)) ,len))])))

(define bad-concat
  (lambda (arr1 arr2)
    (match-let
        ([`(pull ,f1 ,len1) arr1]
         [`(pull ,f2 ,len2) arr2])
      (let ([g (lambda (i)
                 (if (< i len1) (f1 i)
                     (f2 (- i len1))))]
            [len (+ len1 len2)])
        (pull g len)))))