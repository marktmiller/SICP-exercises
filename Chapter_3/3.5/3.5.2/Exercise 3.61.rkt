; Section 3.5 code
; ----------------

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (cons-stream a b)
  (cons a (delay b)))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (delay exp) (memo-proc (lambda () exp)))

(define (force delayed-object) (delayed-object))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      '()
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-null? stream) (null? stream))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define negative-ones (cons-stream -1 negative-ones))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; Let S be a power series whose constant term is 1. Suppose we want to find the
; power series 1/S, that is, the series X such that S * X = 1. Write S = 1 + S(R)
; where S(R) is the part of S after the constant term. Then we can solve for X as
; follows:
;
; S * X = 1
; (1 + S(R)) * X = 1
; X + S(R) * X = 1
; X = 1 - S(R) * X
;
; In other words, X is the power series whose constant term is 1 and whose higher-
; order terms are given by the negative of S(R) times X. Use this idea to write a
; procedure invert-unit-series that computers 1/S for a power series S with
; constant term 1.

(define (mul-series s1 s2)
   (cons-stream (* (stream-car s1) (stream-car s2))
      (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                   (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (cons-stream 1 (mul-streams negative-ones
                              (mul-series (stream-cdr s)
                                          (invert-unit-series s)))))

; test

(define (invert-stream den)
  (cons-stream (/ 1 den) (invert-stream (+ den 1))))

(define (integrate-series s)
  (cons-stream (stream-car s)
               (mul-streams (invert-stream 2)
                            (stream-cdr s))))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

; look for result at position 0
(define test-exp
  (mul-series exp-series (invert-unit-series exp-series)))

; Discussion of result:
;
; This is my second try at this exercise. The first time, I ended up looking up
; the answer somewhere, trying to understand it, and using that (in 2012).
; This time (in 2017) I solved it on my own.
;
; As with Exercise 3.60, the result of test-exp is the scalar value 1 (unit
; series [1, 0, 0, 0, ...]).
;
; As a test, I used the exponential series, since it produces results in all
; positions, which seems to be needed for this function to work.
;
; The progression goes like:
;
; If f() is the series being inverted, and g() is the inverse series (where
; -f(0)^(-1) is just -1):
;
; g(0) = 1
; g(1) = -f(0)^(-1) * f(1)g(0)
; g(2) = -f(0)^(-1) * (f(2)g(0) + f(1)g(1))
; g(3) = -f(0)^(-1) * (f(3)g(0) + f(2)g(1) + f(1)g(2))
;
; As you'll notice, the coefficients of the power series and its inverse "cross
; over" each other. The function invert-unit-series uses the Cauchy product
; method, implemented in mul-series.