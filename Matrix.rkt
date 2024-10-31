#lang racket

(require "Vector.rkt" "helpers.rkt")

(provide )

(define-struct Matrix (rows))
;; A Matrix is either a :
;;   * (make-Matrix (listof Vector))





;; createMatrix: Vector Vector ... Vector Vector -> Matrix
(define createMatrix (lambda rows (make-Matrix rows)))


;; Matrix-dims: Matrix -> (list X Y)
;;   * X is a Num, number of columns
;;   * Y is a Num, number of rows
(define (Matrix-dims mat) (list (length (Vector->list (Matrix-get-row mat 0))) (length (Matrix-rows mat))))


;; Matrix->lists: Matrix -> (listof (listof Nums))
(define (Matrix->lists matrix) (map Vector->list (Matrix-rows matrix)))


;; Matrix-get-row: Matrix Nat -> Vector
(define (Matrix-get-row mat i) (list-ref (Matrix-rows mat) i))


;; Matrix-get-col: Matrix Nat -> Vector
(define (Matrix-get-col mat i) (list->Vector (map (lambda (row) (Vector-get-dim row i)) (Matrix-rows mat))))


;; Matrix-transpose: Matrix -> Matrix
(define (Matrix-transpose mat) (make-Matrix (build-list (first (Matrix-dims mat)) (lambda (i) (Matrix-get-col mat i)))))


;; Matrix-neg: Matrix -> Matrix
(define (Matrix-neg mat) (make-Matrix (map Vector-neg (Matrix-rows mat))))


;; Matrix-add: Matrix Matrix -> Matrix
(define (Matrix-add mat1 mat2) (make-Matrix (map2 Vector-add (Matrix-rows mat1) (Matrix-rows mat2))))


;; Matrix-sub: Matrix Matrix -> Matrix
(define (Matrix-sub mat1 mat2) (Matrix-add mat1 (Matrix-neg mat2)))


;; Matrix-cell-mult: Matrix Matrix -> Matrix
(define (Matrix-cell-mult mat1 mat2) (make-Matrix (map2 Vector-mult (Matrix-rows mat1) (Matrix-rows mat2))))


;; Matrix-mult-Column: Matrix Vector -> Matrix
(define (Matrix-mult-Column mat vec) (Matrix-Col-Vector (list->Vector (map (lambda (row) (Vector-dot row vec)) (Matrix-rows mat)))))


;; Row-mult-Matrix: Vector Matrix -> Matrix
(define (Row-mult-Matrix vec mat) (Matrix-Row-Vector (Matrix-mult-Column (Matrix-transpose mat) vec)))


;; Matrix-mult: Matrix Matrix -> Matrix
(define (Matrix-mult mat1 mat2) (make-Matrix (build-list (first (Matrix-dims mat2)) (lambda (i) (Matrix-mult-Column mat1 (Matrix-get-col mat2 i))))))


;; Matrix-Row-Vector: Vector -> Matrix
(define (Matrix-Row-Vector vec) (createMatrix vec))


;; Matrix-Col-Vector: Vector -> Matrix
(define (Matrix-Col-Vector vec) (Matrix-transpose (createMatrix vec)))