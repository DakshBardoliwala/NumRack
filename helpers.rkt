#lang racket

(provide map2)


;; map2: (X Y -> Z) (listof X) (listof Y) -> (listof Z)
(define (map2 f lst1 lst2) (cond [(empty? lst1) empty] [else (cons (f (first lst1) (first lst2)) (map2 f (rest lst1) (rest lst2)))]))