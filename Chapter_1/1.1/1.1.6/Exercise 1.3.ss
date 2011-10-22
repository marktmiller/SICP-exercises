; Define a procedure that takes three numbers as arguments and returns the sum of
; the squares of the two larger numbers.

(define (sum-of-squares a b) (+ (* a a) (* b b)))
(define (f a b c) (cond ((or (and (> a b) (> b c)) (and (> b a) (> a c)))
                         (sum-of-squares a b))
                         ((or (and (> c a) (> a b)) (and (> a c) (> c b)))
                              (sum-of-squares c a))
                         ((or (and (> c b) (> b a)) (and (> b c) (> c a)))
                          (sum-of-squares c b))))

(f 2 1 3)