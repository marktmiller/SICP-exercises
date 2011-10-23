; Newton's method for cube roots is based on the fact that if y is an approximation
; to the cube root of x, then a better approximation is given by the value:
;
; (x / y^2 + 2y) / 3
;
; Use this formula to implement a cube-root procedure analogous to the square-root
; procedure.

(define (cubert-iter guess previous-guess x)
  (if (good-enough? guess previous-guess)
      guess
      (cubert-iter (improve guess x) guess x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess previous-guess)
  (< (abs (- guess previous-guess)) (/ 1 (if (>= guess 100) guess (* guess 1000)))))

(define (square x)
  (* x x))