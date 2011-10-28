; Define a better version of make-rat that handles both positive and
; negative arguments. Make-rat should normalize the sign so that if
; the rational number is positive, both the numerator and
; denominator are positive, and if the rational number is negative,
; only the numerator is negative.

(define (make-rat num den)
  (cond ((= (* num den) (* (abs num) (abs den)))
         (cons (abs num) (abs den)))
        (else (cons (- (abs num)) (abs den)))))