; The sum procedure in Exercise 1.29 generates a linear recursion. The
; procedure can be rewritten so that the sum is performed iteratively.
; Show how to do this by filling in the missing expressions in the
; following definition:
;
; (define (sum term a next b)
;    (define (iter a result)
;       (if <??>
;           <??>
;           (iter <??> <??>)))
;    (iter <??> <??>))

(define (sum term a next b)
  (define (iter a result)
    (if (= a b)
        (+ result (term a))
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc a)
  (+ a 1))

(define (same a)
  a)

(define (cube x)
  (* x x x))