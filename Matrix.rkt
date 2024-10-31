#lang racket

(require "Vector.rkt" "helpers.rkt")

(provide )

(define-struct Matrix (rows))



(define createMatrix (lambda rows (make-Matrix rows)))

(define (Matrix->lists matrix) (map Vector->list (Matrix-rows matrix)))


(define (Matrix-neg mat) (make-Matrix (map Vector-neg (Matrix-rows mat))))

(define (Matrix-add mat1 mat2) (make-Matrix (map2 Vector-add (Matrix-rows mat1) (Matrix-rows mat2))))

(define (Matrix-sub mat1 mat2) (Matrix-add mat1 (Matrix-neg mat2)))

(define (Matrix-mult mat1 mat2) (make-Matrix (map2 Vector-mult (Matrix-rows mat1) (Matrix-rows mat2))))