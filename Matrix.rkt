#lang racket

(require "Vector.rkt" "helpers.rkt")

(provide )

(define-struct Matrix (rows))



(define createMatrix (lambda rows (make-Matrix rows)))

(define (Matrix-dims mat) (list (length (Vector->list (Matrix-get-row mat 0))) (length (Matrix-rows mat))))

(define (Matrix->lists matrix) (map Vector->list (Matrix-rows matrix)))

(define (Matrix-get-row mat i) (list-ref (Matrix-rows mat) i))

(define (Matrix-get-col mat i) (list->Vector (map (lambda (row) (Vector-get-dim row i)) (Matrix-rows mat))))

(define (Matrix-transpose mat) (make-Matrix (build-list (first (Matrix-dims mat)) (lambda (i) (Matrix-get-col mat i)))))

(define (Matrix-neg mat) (make-Matrix (map Vector-neg (Matrix-rows mat))))

(define (Matrix-add mat1 mat2) (make-Matrix (map2 Vector-add (Matrix-rows mat1) (Matrix-rows mat2))))

(define (Matrix-sub mat1 mat2) (Matrix-add mat1 (Matrix-neg mat2)))

(define (Matrix-cell-mult mat1 mat2) (make-Matrix (map2 Vector-mult (Matrix-rows mat1) (Matrix-rows mat2))))

(define (Matrix-mult-Column mat vec) (Matrix-Col-Vector (list->Vector (map (lambda (row) (Vector-dot row vec)) (Matrix-rows mat)))))

(define (Row-mult-Matrix vec mat) (Matrix-Row-Vector (Matrix-mult-Column (Matrix-transpose mat) vec)))

(define (Matrix-mult mat1 mat2) (make-Matrix (build-list (first (Matrix-dims mat2)) (lambda (i) (Matrix-mult-Column mat1 (Matrix-get-col mat2 i))))))

(define (Matrix-Row-Vector vec) (createMatrix vec))

(define (Matrix-Col-Vector vec) (Matrix-transpose (createMatrix vec)))