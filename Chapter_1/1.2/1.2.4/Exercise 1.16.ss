; Design a procedure that evolves an iterative exponentiation process that uses
; successive squaring and uses a logarithmic number of steps, as does
; fast-expt.

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (fast-expt-even b b n))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-even accum base n)
  (define (fast-expt-iter accum count)
    (cond ((> (/ n 2) (* count 2)) (fast-expt-iter (* accum accum) (* count 2)))
          ((= (/ n 2) count) (* accum accum))
          (else (fast-expt-iter (* accum (fast-expt base (- (/ n 2) count)))
                                (+ count (- (/ n 2) count))))))
  (fast-expt-iter accum 1))
