#lang racket

(require "pull.rkt")
(require "push.rkt")

(provide (all-defined-out))

;; array fission

(define split
  (lambda (arr)
    (match arr
      [`(pull ,f ,len)
       (let ([a (pull f (ceiling (/ len 2)))]
             [b (pull (lambda (i)
                        (f (+ i (ceiling (/ len 2)))))
                      (floor (/ len 2)))])
         `(,a ,b))])))

(define fission-map
  (lambda (f arr)
    (match arr
      [`(pull ,g ,len)
       (match-let ([`(,a ,b) (split arr)])
         `(concat ,(map f a) ,(map f b)))]
      [`(concat ,a ,b)
       `(concat ,(map f a) ,(map f b))])))

(define fission-compute
  (lambda (arr)
    (match arr
      [`(pull ,f ,len)
       (pull->vector arr)]
      [`(concat ,a ,b)
       (let ([a (pull->push a)]
             [b (pull->push b)])
         (pcompute (concat a b)))])))

(define fission-fold
  (lambda (f init arr)
    (match arr
      [`(push ,g ,len)
       (match-let ([`(,a ,b) (split arr)])
         (f (fold f init a) (fold f init b)))]
      [`(concat ,a ,b)
       (f (fold f init a) (fold f init b))])))
