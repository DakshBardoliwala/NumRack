#lang racket

(provide createVector Vector->list Vector-add Vector-mult Vector-neg list->Vector Vector-dot Vector-get-dim)

(require "helpers.rkt")

(define-struct Vector (coords))
;; A Vector is either a :
;;   * (make-Vector (listof Num))



;; createVector: Num Num ... Num Num -> Vector
(define createVector (lambda coords (make-Vector coords)))

(define (Vector->list vec) (Vector-coords vec))
(define (list->Vector coords) (make-Vector coords))

;; Vector-x, Vector-y, Vector-z: Vector -> Num
(define (Vector-x vec) (first (Vector-coords vec)))
(define (Vector-y vec) (second (Vector-coords vec)))
(define (Vector-z vec) (third (Vector-coords vec)))

;; Vector-magSq, Vector-mag: Vector -> Num
(define (Vector-magSq vec) (foldr (lambda (x y) (+ y (* x x))) 0 (Vector-coords vec)))
(define (Vector-mag vec) (sqrt (Vector-magSq vec)))

;; Vector-all-1: Nat -> Vector
(define (Vector-all-1 dim) (make-Vector (build-list dim (lambda (x) 1))))

;; Vector-one-dir: Nat Nat -> Vector
(define (Vector-one-dir dim i) (make-Vector (build-list dim (lambda (x) (cond [(= i x) 1] [else 0])))))

;; Vector-dims: Vector -> Nat
(define (Vector-dims vec) (length (Vector-coords vec)))

;; Vector-get-dim: Vector Nat -> Num
(define (Vector-get-dim vec dim) (list-ref (Vector-coords vec) dim))

;; Vector-add: Vector Vector -> Vector

(define (Vector-add vec1 vec2) (make-Vector (map2 + (Vector->list vec1) (Vector->list vec2))))

#|
(define (Vector-add vec1 vec2) (make-Vector (map (lambda (i) (cond [(>= i (Vector-dims vec1)) (Vector-get-dim vec2 i)]
                                                      [(>= i (Vector-dims vec2)) (Vector-get-dim vec1 i)]
                                                      [else (+ (Vector-get-dim vec1 i) (Vector-get-dim vec2 i))]))
                                    (build-list (max (Vector-dims vec1) (Vector-dims vec2)) (lambda (x) x)))))
|#


;; Vector-neg: Vector -> Vector
(define (Vector-neg vec) (make-Vector (map (lambda (x) (- 0 x)) (Vector-coords vec))))

;; Vector-sub: Vector Vector -> Vector
(define (Vector-sub vec1 vec2) (Vector-add vec1 (Vector-neg vec2)))

;; DEFINE VECTOR-MAP

;; Vector-mult: Vector Vector -> Vector
(define (Vector-mult vec1 vec2) (make-Vector (build-list (max (Vector-dims vec1) (Vector-dims vec2)) (lambda (i) (if (or (>= i (Vector-dims vec1)) (>= i (Vector-dims vec1)))
                                                                                                        0
                                                                                                        (* (Vector-get-dim vec1 i) (Vector-get-dim vec2 i)))))))

;; Vector-manhattan: Vector -> Num
(define (Vector-manhattan vec) (foldr + 0 (Vector-coords vec)))

;; Vector-dot: Vector Vector -> Num
(define (Vector-dot vec1 vec2) (Vector-manhattan (Vector-mult vec1 vec2)))

;; Vector-cross: Vector Vector -> Vector
(define (Vector-cross vec1 vec2) (createVector (- (* (Vector-y vec1) (Vector-z vec2)) (* (Vector-z vec1) (Vector-y vec2)))
                                               (- (* (Vector-z vec1) (Vector-x vec2)) (* (Vector-x vec1) (Vector-z vec2)))
                                               (- (* (Vector-x vec1) (Vector-y vec2)) (* (Vector-y vec1) (Vector-x vec2)))))


;; Vector-unit: Vector -> Vector
(define (Vector-unit vec) (Vector-scale vec (/ 1 (Vector-mag vec))))

;; Vector-scale, Vector-setMag: Vector Num -> Vector
(define (Vector-scale vec num) (make-Vector (map (lambda (x) (* x num)) (Vector-coords vec))))
(define (Vector-setMag vec mag) (Vector-scale (Vector-unit vec) mag))

;; Vector-normal: Vector -> Vector
(define (Vector-normal vec)
  (Vector-sub
   (Vector-setMag (Vector-one-dir (Vector-dims vec) 0) (Vector-manhattan vec))
   (Vector-scale (Vector-all-1 (Vector-dims vec)) (Vector-x vec))))

