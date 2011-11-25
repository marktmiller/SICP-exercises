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

; Use the results of exercises 3.60 and 3.61 to define a procedure div-series
; that divides two power series. Div-series should work for any two series,
; provided that the denominator series begins with a nonzero constant term.
; (If the denominator has a zero constant term, then div-series should
; signal an error.) Show how to use div-series together with the result of
; exercise 3.59 to generate the power series for tangent.

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

(define (div-series s1 s2)
  (if (not (= (stream-car s2) 0))
      (mul-series s1 (invert-unit-series s2))
      (error "Denominator series has a zero constant term -- div-series")))

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

; Use (stream-ref tan-series <position>) to test
(define tan-series (div-series sine-series cosine-series))

; As with sine-series, the coefficients are all in the odd positions in the
; stream.