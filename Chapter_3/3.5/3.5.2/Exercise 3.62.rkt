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

(define (mul-series s1 s2)
   (cons-stream (* (stream-car s1) (stream-car s2))
      (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                   (mul-series (stream-cdr s1) s2))))

(define (invert-unit-series s)
  (cons-stream 1 (mul-streams negative-ones
                              (mul-series (stream-cdr s)
                                          (invert-unit-series s)))))

; Use the results of exercises 3.60 and 3.61 to define a procedure div-series
; that divides two power series. Div-series should work for any two series,
; provided that the denominator series begins with a nonzero constant
; term. (If the denominator has a zero constant term, then div-series should
; signal an error.) Show how to use div-series together with the result of
; exercise 3.59 to generate the power series for tangent.

(define (div-series s1 s2)
  (cond ((= (stream-car s2) 0)
         (display "Error - Divisor series has 0 constant term")
         -1)
        (else (mul-series s1 (invert-unit-series s2)))))

; test

(define (invert-stream den)
  (cons-stream (/ 1 den) (invert-stream (+ den 1))))

(define (integrate-series s)
  (cons-stream (stream-car s)
               (mul-streams (invert-stream 2)
                            (stream-cdr s))))

(define cosine-series
  (cons-stream 1 (integrate-series (mul-streams negative-ones sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; Should be [0, 1, 0, 1/3, 0, 2/15, 0, 17/315, ...]
(define tan-series (div-series sine-series cosine-series))

; Discussion of results:
;
; Since tan x = sin x / cos x, we divide sine-series by cosine-series. Tan x
; is an odd series (the powers are 1, 3, 5, ..., with no constant
; term), so the coefficients will be in the odd positions in the stream.