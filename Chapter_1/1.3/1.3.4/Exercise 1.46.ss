; Iterative improvement says that, to compute something, we start with
; an initial guess and continue the process using the improved guess
; as the new guess. Write a procedure iterative-improve that takes
; two procedures as arguments: a method for telling whether a guess
; is good enough and a method for improving the guess.
; Iterative-improve should return as its value a procedure that
; takes a guess as argument and keeps improving the guess until it
; is good enough. Rewrite the sqrt procedure of section 1.1.7 and
; the fixed-point procedure of section 1.3.3 in terms of
; iterative-improve.

; Section 1.1.7 code
; -------------------
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

;(define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (sqrt-iter (improve guess x) x)))

; Section 1.3.3 code
; ------------------
(define tolerance 0.00001)
;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2)) tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;          next
;          (try next))))
;  (try first-guess))

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

; ------------------------------------

(define (square x)
  (* x x))

(define (iterative-improve ge ig)
  (lambda (guess)
    (define (try v1 v2)
      (if (ge v1 v2) v2 (try v2 (ig v2))))
    (try guess (ig guess))))

(define (fixed-point f first-guess)
  ((iterative-improve close-enough? (lambda (guess) (f guess))) first-guess))

(define (sqrt-iter guess x)
  ((iterative-improve (lambda (extra guess) (good-enough? guess x))
                      (lambda (guess) (improve guess x))) guess))

(define (sqrt2 x) (sqrt-iter 1.0 x))