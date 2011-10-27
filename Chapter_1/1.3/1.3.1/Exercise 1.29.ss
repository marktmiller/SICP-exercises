; Simpson's Rule is a more accurate method of numerical integration than the
; method illustrated in Section 1.3.1. Using Simpson's Rule, the integral of
; a function f between a and b is approximated as:
;
; (h / 3) * (y(0) + 4y(1) + 2y(2) + 4y(3) + 2y(4) + ... +
;    2y(n-2) + 4y(n-1) + y(n))
;
; where h = (b - a)/n, for some even integer n, and y(k) = f(a + kh).
; (Increasing n increases the accuracy of the approximation.) Define a
; procedure that takes as arguments f, a, b, and n and returns the value
; of the integral, computed using Simpson's Rule. Use your procedure to
; integrate cube between 0 and 1 (with n = 100 and n = 1000), and compare
; the results to those of the integral procedure shown in Section 1.3.1.

; Section 1.3.1 code
; ------------------

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

; ----------------------------------------------

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (* (/ h 3.0) (+ (f a) (simpson-sum f 1 n 4 a h) (f b))))

(define (simpson-sum f k n c a h)
  (+ (* c (f (+ a (* h k))))
     (if (< k n)
         (simpson-sum f (+ k 1) n (if (= c 4) 2 4) a h)
         0)))

(define (cube x)
  (* x x x))