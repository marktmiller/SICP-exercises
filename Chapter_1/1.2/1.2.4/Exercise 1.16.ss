; Design a procedure that evolves an iterative exponentiation process that uses
; successive squaring and uses a logarithmic number of steps, as does
; fast-expt.

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (fast-expt-iter-even2 b b n 1))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter-even2 accum base n count)
  (cond ((> (/ n 2) (* count 2))
         (fast-expt-iter-even2 (* accum accum) base n (* count 2)))
        ((= (/ n 2) count) (* accum accum))
        (else (fast-expt-iter-even2
               (* accum (fast-expt base (- (/ n 2) count))) base n
               (+ count (- (/ n 2) count))))))