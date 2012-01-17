; Must be run in Lazy Scheme

; Define a procedure partial-sums that takes as argument a stream S
; and returns the stream whose elements are S(0), S(0) + S(1),
; S(0) + S(1) + S(2),.... For example, (partial-sums integers)
; should be the stream 1, 3, 6, 10, 15, ...

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

(define (delay exp) (lambda () exp))

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

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

; -------------------

(define (partial-sums s)
  (cons-stream (stream-car s) (add-streams (partial-sums s)
                                           (stream-cdr s))))