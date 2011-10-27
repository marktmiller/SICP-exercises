; Devise a procedure that generates an iterative process for multiplying
; two integers in terms of adding, doubling, and halving and uses a
; logarithmic number of steps.

(define (mult a b)
  (cond ((= a 0) 0)
        ((= b 0) 0)
        ((= a 1) b)
        ((= b 1) a)
        ((= a -1) (- b))
        ((= b -1) (- a))
        (else (mult2 a (if (even? b) b (+ b (- 1))) (if (even? b) 0 a)))))

(define (mult2 a b remainder)
  (define oa a)
  (define (mult-iter accum count)
    (cond ((< (double count) (halve b)) (mult-iter (double accum) (double count)))
          ((= count (halve b)) (+ (double accum) remainder))
          (else (mult-iter (+ accum (mult oa (+ (halve b) (- count))))
                           (+ count (+ (halve b) (- count)))))))
  (mult-iter a 1))

(define (halve a)
  (/ a 2))

(define (double a)
  (* 2 a))
