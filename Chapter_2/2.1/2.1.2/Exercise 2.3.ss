; Implement a representation for rectangles in a plane. (Hint: You may
; want to make use of exercise 2.2.) In terms of your constructors
; and selectors, create procedures that compute the perimeter and
; the area of a given rectangle. Now implement a different
; representation for rectangles. Can you design your system with
; suitable abstraction barriers, so that the same perimeter and
; area procedures will work using either representation?

; From Exercise 2.2
;------------------
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

;------------------------------------------------
; Exercise 2.3

; (define pi 3.14159)

; Note: dir is in radians
(define (make-vect dir len)
  (cons dir len))

; Using some knowledge from the next section...

(define (make-rectangle-pt foc1 foc2)
  (define rect (cons foc1 foc2))
  (define (dispatch n)
    (cond ((= n 0)
           (abs (- (x-point (focus2 rect)) (x-point (focus1 rect)))))
          ((= n 1)
           (abs (- (y-point (focus2 rect)) (y-point (focus1 rect)))))))
  dispatch)

(define (make-rectangle-vect foc1 foc2)
  (define rect (cons foc1 foc2))
  (define (dispatch n)
    (cond ((= n 0) (len (focus1 rect)))
          ((= n 1) (len (focus2 rect)))))
  dispatch)

(define (major-length rect) (rect 0))
(define (minor-length rect) (rect 1))

(define (focus1 rect)
  (car rect))

(define (focus2 rect)
  (cdr rect))

(define (dir vect)
  (car vect))

(define (len vect)
  (cdr vect))

(define (perimeter rect)
  (+ (* 2 (major-length rect)) (* 2 (minor-length rect))))

(define (area rect)
  (* (major-length rect) (minor-length rect)))
