#lang racket

(provide (all-defined-out))

;; push arrays

(define push
  (lambda (f len)
    `(push ,f ,len)))

(define pcompute
  (lambda (arr)
    (match arr
      [`(push ,f ,len)
        (let ([v (make-vector len)])
          (f (lambda (i a) (vector-set! v i a)))
          v)])))

(define pull->push
  (lambda (arr)
    (match arr
     [`(pull ,f ,len)
       (let ([r (lambda (w)
                  (let loop ([i 0])
                    (if (= i len) (void)
                        (begin
                          (w i (f i))
                          (loop (add1 i))))))])
         (push r len))])))

(define concat
  (lambda (arr1 arr2)
    (match-let
        ([`(push ,f1 ,len1) arr1]
         [`(push ,f2 ,len2) arr2])
      (let ([r (lambda (w)
                 (f1 w)
                 (f2 (lambda (i a) 
                       (w (+ i len1) a))))])
        (push r (+ len1 len2))))))

(define dup
  (lambda (arr)
    (match arr
      [`(push ,f ,len)
        (let ([r (lambda (w)
                   (f (lambda (i a)
                        (w (* 2 i) a)
                        (w (+ (* 2 i) 1) a))))])
          (push r (* 2 len)))])))  

