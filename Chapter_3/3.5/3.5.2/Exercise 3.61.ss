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
          (being (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (delay exp) (memo-proc (lambda () exp)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      '()
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-null? stream) (null? stream))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map + s1 s2))

; ------------------

; Let S be a power series whose constant term is 1. Suppose we want to find
; the power series 1/S, that is, the series X such that S * X = 1.
; Write S = 1 + S(R) where S(R) is the part of S after the constant term.
; Then we can solve for X as follows:
;
; S * X = 1
; (1 + S(R)) * X = 1
; X + S(R) * X = 1
; X = 1 - S(R) * X
;
; In other words, X is the power series whose constant term is 1 and whose
; higher-order terms are given by the negative of S(R) times X. Use this
; idea to write invert-unit-series that computes 1/S for a power series S
; with constant term 1.

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define negative-ones (cons-stream -1 negative-ones))

(define (scalar-mul-series v s)
  (cons-stream (* v (stream-car s)) (scalar-mul-series v (stream-cdr s))))

; From http://wqzhang.wordpress.com/2009/08/10/sicp-exercise-3-60/
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                (add-streams (scalar-mul-series (stream-car s1) (stream-cdr s2))
                             (scalar-mul-series (stream-car s2) (stream-cdr s1)))
                (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(define (invert-unit-series s)
  (cons-stream 1 (mul-series
                  (mul-streams negative-ones (stream-cdr s))
                  (invert-unit-series s))))

; Using Exercise 3.59 as test code
; --------------------------------

(define (invert-stream den)
  (cons-stream (/ 1 den) (invert-stream (+ den 1))))

(define (integrate-series s)
  (cons-stream (stream-car s)
               (mul-streams (invert-stream 2)
                            (stream-cdr s))))

; Test by using (mul-series cosine-series (invert-unit-series cosine-series))
(define cosine-series
  (cons-stream 1 (integrate-series (mul-streams negative-ones sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))