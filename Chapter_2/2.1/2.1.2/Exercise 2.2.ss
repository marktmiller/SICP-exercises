; Consider the problem of representing line segments in a plane. Each
; segment is represented as a pair of points: a starting point and an
; ending point. Define a constructor make-segment and selectors
; start-segment and end-segment that define the representation of
; segments in terms of points. Furthermore, a point can be represented
; as a pair of numbers: the x coordinate and the y coordinate.
; Accordingly, specify a constructor make-point and selectors x-point
; and y-point that define this representation. Finally, using your
; selectors and constructors, define a procedure midpoint-segment
; that takes a line segment as argument and returns its midpoint
; (the point whose coordinates are the average of the coordinates
; of the endpoints).

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define (x-point pt)
  (car pt))

(define (y-point pt)
  (cdr pt))

; from Exercise 2.2 text
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (avg-component seg selector)
  (define start (start-segment seg))
  (define end (end-segment seg))
  (/ (+ (selector start) (selector end)) 2.0))

(define (midpoint-segment seg)
  (make-point (avg-component seg x-point)
              (avg-component seg y-point)))
