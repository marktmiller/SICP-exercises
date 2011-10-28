; Show that we can represent pairs of nonnegative integers using only
; numbers and arithmetic operations if we represent the pair a and b
; as the integer that is the product of 2^a * 3^b. Give the
; corresponding definitions of the procedures cons, car, and cdr.

(define (mathcons a b)
  (* (expt 2 a) (expt 3 b)))

(define (mathcar pair)
  (if (= (gcd pair 2) 2) (+ 1 (mathcar (/ pair 2))) 0))

(define (mathcdr pair)
  (if (= (gcd pair 3) 3) (+ 1 (mathcdr (/ pair 3))) 0))