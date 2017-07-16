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

; Using Cauchy product
(define (mul-series s1 s2)
   (cons-stream (* (stream-car s1) (stream-car s2))
      (add-streams (stream-map (lambda (x) (* (stream-car s1) x))
                               (stream-cdr s2))
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

; look for result in position 0
(define cos-sin-squared-series
  (add-streams (mul-series sine-series sine-series)
               (mul-series cosine-series cosine-series)))

; Discussion of result:
;
; Since the result of sin^2(x) + cos^2(x) is a scalar value (1), this
; produces a unit series of [1, 0, 0, 0, ...].
;
; This is my second attempt at this exercise. The first time (in 2012),
; I remember kind of getting it right, but not totally, and I ended up looking
; up the answer, and using that. This time (in 2017), I solved it on my own.
;
; The way mul-series works is by using the Cauchy product method. This is really
; the only way it can work, since it automatically combines like terms as the
; multiplication happens. The Cauchy product uses partial sums of products,
; creating a "wedge" of additions:
;
; a0b0
; a1b0 + a0b1
; a2b0 + a1b1 + a0b2
; ...
;
; Notice the triangular shape. Each line of additions represents a
; coefficient of a term in the multiplied series. Notice that the two series
; "cross over" each other as the partial sums of products are produced, with
; the first series (a) starting at a later term, and counting down, and the
; second series (b) always starting at the 0th term, and counting up.
;
; Since the streams architecture does not allow going backwards through a
; stream, the only option was to find a way to do the sums of products while
; moving forard. The method I found was to "go diagonally and down." The
; vertical is computed in the product at the car of the cons-stream,
; computing (a0b0 a1b0 a2b0 ...). The diagonal is computed in
; the call to stream-map. This computes the following sequences:
; (a0b1 a0b2 a0b3 ...), then (a1b1 a1b2 a1b3 ...), etc. (notice these
; sequences along the diagonals of the Cauchy product above). These two
; streams are summed by add-streams.