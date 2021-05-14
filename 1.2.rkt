#lang racket

; 1.2.1
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (sub1 n)))))

(define (factorial-iter n)
  (define (iter product i)
    (if (> i n)
        product
        (iter (* i product) (add1 i))))
  (iter 1 1))

; Exercise 1.9
;; (define (+ a b) ;; recursive
;; (if (= a 0) b (inc (+ (dec a) b))))

;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (+ 0 5))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)

;; 9

;;(define (+ a b) ;; iterative
;;  (if (= a 0) b (+ (dec a) (inc b))))

;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)

;; 9

; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;; (A 1 10)
;; 1024

;; (A 2 4)
;; 65536

;; (A 3 3)
;; 65536

(define (f n) (A 0 n))
;; f(n) = 2 * y
(define (g n) (A 1 n))
;; g(n) = 2 ** n
(define (h n) (A 2 n))
;; h(n) = 
(define (k n) (* 5 n n))
;; k(n) = 5 * n ** 2

; 1.2.2 Tree Recursion

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(define (fib-iter n)
  (define (iter a b i)
    (if (= i 0)
        b
        (iter (+ a b) a (sub1 i))))
  (iter 1 0 n))

; Example: Counting change
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                         kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; Exercise 1.11
(define (f111 n)
  (if (< n 3)
      n
      (+ (f111 (- n 1))
         (* 2 (f111 (- n 2)))
         (* 3 (f111 (- n 3))))))

(define (f111-iter n)
  (define (iter a b c i)
    (if (= i 0)
        c
        (iter (+ a (* 2 b) (* 3 c)) a b (- i 1))))
  (iter 2 1 0 n))

; Exercise 1.12
(define (pascal row col)
  (cond ((> col row) (raise "invalid input"))
        ((or (<= row 1)
             (= col 0)
             (= col row)) 1)
        (else (+ (pascal (sub1 row)
                         (sub1 col))
                 (pascal (sub1 row)
                         col)))))
(define (show-pascal n)
  (define (iter i j)
    (cond ((= i 0) (println (pascal i j)))
          (else (println (pascal i j))
                (iter (sub1 i) (sub1 j)))))
  (iter n n))

; Exercise 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (println angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

; 1.24
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (sub1 n)))))
(define (expt-iter b n)
  (define (iter i product)
    (if (= i 0)
        product
        (iter (sub1 i) (* b product))))
  (iter n 1))

(define (square x)
  (* x x))
(define (fast-expt b n)
  (println n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; Exercise 1.16
(define (fast-expt-iter base n)
  (define (iter i b a)
    (cond ((= i 0) a)
          ((even? i) (iter (/ i 2)
                           (square b)
                           a))
          (else (iter (sub1 i)
                      b
                      (* b a)))))
  (iter n base 1))

; Exercise 1.17
(define (halve x) (/ x 2))
(define (double x) (+ x x))
(define (fast-mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mul a (halve b))))
        (else (+ a (fast-mul a (sub1 b))))))

; Exercise 1.18
(define (fast-mul-iter multiplicand multiplier)
  (define (iter i b a)
    (cond ((= i 0) a)
          ((even? i) (iter (halve i) (double b) a))
          (else (iter (sub1 i) b (+ a b)))))
  (iter multiplier multiplicand 0))

; Exercise 1.19
