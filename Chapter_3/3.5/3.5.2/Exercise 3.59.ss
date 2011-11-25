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

;---------------------------

; Part a. - The integral of the series a(0) + a(1)x + a(2)x^2 + a(3)x^3 + ...
; is the series
;
; c + a(0)x + 1/2(a(1)x^2) + 1/3(a(2)x^3) + 1/4(a(3)x^4) + ...
;
; where c is any constant. Define a procedure integrate-series that takes as
; input a stream a(0), a(1), a(2), ... representing a power series and returns
; the stream a(0), (1/2)a(1), (1/3)a(2), ... of coefficients of the
; non-constant terms of the integral of the series. (Since the result has no
; constant term, it doesn't represent a power series; when we use
; integrate-series, we will cons on the appropriate constant.)

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (invert-stream den)
  (cons-stream (/ 1 den) (invert-stream (+ den 1))))

(define (integrate-series s)
  (cons-stream (stream-car s)
               (mul-streams (invert-stream 2)
                            (stream-cdr s))))

; Part b. - Show how to generate the series for sine and cosine, starting
; from the facts that the derivative of sine is cosine and the derivative
; of cosine is the negative of sine:
;
; cos x = 1 - (x^2)/2! + (x^4)/4! - ...
;
; sin x = x - (x^3)/3! + (x^5)/5! - ...
;
; (define cosine-series
;    (cons-stream 1 <??>))
; (define sine-series
;    (cons-stream 0 <??>))

; Well...they gave an awfully big hint here. I can see they were not trying
; to turn this into a hard math problem. They just wanted students to think
; a bit about this logically.
;
; Since I know that integration, for the purposes of this exercise, does the
; inverse of derivation, I set up the streams to integrate the derivatives
; of sine and cosine, using mutual stream references. I had to add in a
; "negation stream" (negative-ones) to negate the sine series for cosine, but
; otherwise the solution was straightforward. The initial conditions were
; tricky, because I didn't understand what they were getting at starting
; the streams with 1 and 0, since this didn't appear to match what the
; first values in the sequences should be, given what the values are in the
; series. I explain this below.

(define negative-ones (cons-stream -1 negative-ones))

(define cosine-series
  (cons-stream 1 (integrate-series (mul-streams negative-ones sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

; Discussion of results
;
; Working with the cosine and sine series streams can be confusing. Here is why
; they work. You have to think of the results from the two streams as being
; *positional*. As the description says above, integrate-series always
; produces coefficients according to the following sequence:
;
; a(0)x, (1/2)a(1)x^2, (1/3)a(2)x^3, ...
;
; Though here the x terms are what's important. The constant term in the
; power series is prepended to the stream before integrate-series is called.
;
; The cosine-series only produces coefficients in the even positions. In
; other words, you have to call stream-ref with even position values (which
; includes position 0). The odd positions will always be 0.
; Theoretically, this is because the exponents in the series are even
; (it has no x or x^3 terms, for example).
;
; The sine-series is the opposite. All of the coefficient values are in the
; odd positions in the stream (starting at position 1). This is for the same
; reason. All of the exponents are odd. It has no constant term, so the very
; first position gives you 0.