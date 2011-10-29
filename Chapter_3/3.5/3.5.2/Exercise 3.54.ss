; Must be run in Lazy Scheme

; Define a procedure mul-streams, analogous to add-streams, that
; produces the elementwise product of its two input streams.
;
; Basically, use this with the integers stream to create a stream
; whose nth element (counting from 0) is n + 1 factorial.

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

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

;---------------------------

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials (cons-stream 1 (mul-streams (stream-cdr integers)
                                               factorials)))