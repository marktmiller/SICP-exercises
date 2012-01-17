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

(define ones (cons-stream 1 ones))

;---------------------------

(define (mul-streams s1 s2) (stream-map * s1 s2))

; With power series represented as streams of coefficients as in exercise 3.59,
; adding series is implemented by add-streams. Complete the definition of the
; following procedure for multiplying series:
;
; (define (mul-series s1 s2)
;    (cons-stream <??> (add-streams <??> <??>)))
;
; You can test your procedure by verifying that sin^2(x) + cos^2(x) = 1,
; using the series from exercise 3.59.

(define (scalar-mul-series v s)
  (cons-stream (* v (stream-car s)) (scalar-mul-series v (stream-cdr s))))

(define (mul-series s1 s2)
  (cons-stream 0 (add-streams (scalar-mul-series (stream-car s1) s2)
                              (mul-series (stream-cdr s1) s2))))

; Test code from Exercise 3.59
; ----------------------------

(define (invert-stream den)
  (cons-stream (/ 1 den) (invert-stream (+ den 1))))

(define (integrate-series s)
  (cons-stream (stream-car s)
               (mul-streams (invert-stream 2)
                            (stream-cdr s))))

(define negative-ones (cons-stream -1 negative-ones))

(define cosine-series
  (cons-stream 1 (integrate-series (mul-streams negative-ones sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; Discussion of results
;
; This problem didn't lend itself to creating a mathematically elegant
; solution. It turned out I needed to break out the arithmetic more than
; I did. My solution works, though the representation is wrong. The
; correct output from the stream can be seen in the odd positions. The
; even positions are always 0. The correct answer of 1 (for the test 
; sine^2(x) + cos^2(x) = 1) comes out at position 1. All the other
; positions come out 0 as they should.
;
; You can see a correct version of mul-series at:
;
; http://wqzhang.wordpress.com/2009/08/10/sicp-exercise-3-60/