; Suppose we include, together with addition, operations double, which
; doubles an integer, and halve, which divides an (even) integer by 2.
; Using these, design a multiplication procedure analogous to fast-expt
; that uses a logarithmic number of steps.

(define (double a)
  (* 2 a))

(define (halve a)
  (/ a 2))

(define (mult-even a b)
  (cond ((or (= a 0) (= b 0)) 0)
        ((>= (abs a) 2) (double (mult2 (halve a) b)))))

(define (mult2 a b)
  (cond ((even? a) (mult-even a b))
        (else (+ (mult-even (- a 1) b) b))))