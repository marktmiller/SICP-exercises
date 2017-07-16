; Requires Lazy Scheme

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

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; Write a procedure stream-limit that takes as arguments a stream and
; a number (the tolerance). It should examine the stream until it
; finds two successive elements that differ in absolute value by less
; than the tolerance, and return the second of the two elements.
; Using this, we could compute square roots up to a given tolerance by
;
; (define (sqrt x tolerance)
;    (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit stream tolerance)
  (if (< (abs (- (stream-car stream) (stream-car (stream-cdr stream))))
         tolerance)
      (stream-car (stream-cdr stream))
      (stream-limit (stream-cdr stream) tolerance)))

(define (sqrt-limit x tolerance)
  (stream-limit (sqrt-stream x) tolerance))