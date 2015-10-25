#lang racket

(provide (all-defined-out))

;; staged pull arrays

(define spull 
  (lambda (f len) 
    `(pull ,f ,len)))

(define s*
  (lambda (a b) `(* ,a ,b)))
(define s+
  (lambda (a b) `(+ ,a ,b)))
(define sadd1
  (lambda (a) `(add1 ,a)))

(define scompute
  (lambda (arr)
    (match arr
      [`(pull ,f ,len)
       (let ([i (gensym 'i)])
         `(build-vector
           ,len
           (lambda (,i) ,(f i))))])))

(define szipwith
  (lambda (f arr1 arr2)
    (match-let
        ([`(pull ,f1 ,len1) arr1]
         [`(pull ,f2 ,len2) arr2])
      (let ([g (lambda (i)
                 (f (f1 i) (f2 i)))]
            [len (min len1 len2)])
        (spull g len)))))

(define sfold
  (lambda (f init arr)
    (match arr
      [`(pull ,g ,len)
        (let ([acc (gensym 'acc)]
              [i (gensym 'i)]
              [loop (gensym 'loop)])
          `(let ,loop ([,acc ,init] [,i 0])
             (if (= ,i ,len) ,acc
                 (,loop ,(f acc (g i))
                        (add1 ,i)))))])))