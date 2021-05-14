#lang racket

; 1.1

(define (square x)
  (* x x))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

; Exercise 1.1
;; 10
10

;; (+ 5 3 4)
12

;; (- 9 1)
8

;; (/ 6 2)
3

;; (+ (* 2 4) (- 4 6))
6

;; (define a 3)

;; (define b (+ a 1))

;; (+ a b (* a b))
19

;; (= a b)
#f

;; (if (and (> b a) (< b (* a b)))
;;     b
;;     a)
4

;; (cond ((= a 4) 6)
;;       ((= b 4) (+ 6 7 a))
;;       (else 25))
16

;; (+ 2 (if (> b a) b a))
6

;; (* (cond ((> a b) a)
;;          ((< a b) b)
;;          (else -1))
;;    (+ a 1))
16

; Exercise 1.2
(/ (+ 5
      4
      (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3
       (- 6 2)
       (- 2 7)))

; Exercise 1.3
(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

(define (sum-of-max-squares x y z)
  (cond ((and (> x z) (> y z)) (sum-of-squares x y))
        ((and (> x y) (> z y)) (sum-of-squares x z))
        (else (sum-of-squares y z))))

; Exercise 1.4
;; The if operator body returns an expression that returns a procedure
;; + or -, which will be applied to a and b.

; Exercice 1.5
;; In applicative-order the evaluation never ends
;; (test 0 (p))
;; (test 0 (p))
;; ...

;; In normal-order (p) parameter is never evaluated
;; (test 0 (p))
;; (if (= 0 0) 0 (p))
;; (if #t 0 (p))
;; 0

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (= (improve guess x) guess))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; Exercise 1.6
;; The program never stops, because of the recursive nature of this
;; procedure the 3rd parameter will always be evaluated. which is a recursive call

; Exercie 1.8
(define (cbrt-improve guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (cbrt-good-enough? guess x)
  (= (cbrt-improve guess x) guess))

(define (cbrt-iter guess
                   x)
  (if (cbrt-good-enough? guess
                         x)
      guess
      (cbrt-iter (cbrt-improve guess
                               x)
                 x)))

(define (cbrt x)
  (cbrt-iter 1.1 x))