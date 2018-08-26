; A function f is defined by the rule that:
; f(n) = n if n < 3 and
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3.
;
; Write a procedure that computes f by means of an iterative process.

(define (f n)
  (f-iter 1 n 0 0 0))

(define (f-iter count n v1 v2 v3)
  (cond ((>= count n) (if (< n 3) n (+ v1 (* 2 v2) (* 3 v3))))
        ((< count 3) (f-iter (+ count 1) n count v1 v2))
        (else (f-iter (+ count 1) n (+ v1 (* 2 v2) (* 3 v3)) v1 v2))))