; Suppose we want to modify the differentiation program so that it
; works with ordinary mathematical notation, in which + and * are
; infix rather than prefix operators. Since the differentiation
; program is defined in terms of abstract data, we can modify it to
; work with different representations of expressions solely by
; changing the predicates, selectors, and constructors that define
; the representation of the algebraic expressions on which the
; differentiator is to operate.
;
; The problem becomes substantially harder if we allow standard
; algebraic notation, such as (x + 3 * (x + y + 2)), which drops
; unnecessary parentheses and assumes that multiplication is done
; before addition. Design appropriate predicates, selectors, and
; constructors for this notation such that the derivative program
; still works.
;
; The method I used was to process sums of products, and only consider
; the expression to be a product if there were no sums in it. In
; addition, I used "addend" and "augend" rather like "car" and
; "cdr" operations on the sum of products. It breaks down an
; expression like:
;
; (deriv '(2 * x * (3 * x + y) + y + 3) 'x)
;
; into:
;
; 1. (sum) (make-sum (deriv (2 * x * (3 * x + y)) x) (deriv (y + 3) x))
;
; 2. (product) (deriv (2 * x * (3 * x + y)) x)
; (make-sum (make-product 2 (deriv (x * (3 * x + y)) x))
;           (make-product (deriv 2 x) (x * (3 * x + y))))
;
; 3. (product) (deriv (x * (3 * x + y)) x)
; (make-sum (make-product x (deriv (3 * x + y) x))
;           (make-product (deriv x x) (3 * x + y)))
;
; 4. (sum) (deriv (3 * x + y) x)
; (make-sum (deriv (3 * x) x) (deriv y x))
; Result for (4) is: 3
;
; Result for (3) is (x * 3 + 3 * x + y)
;
; Result for (1) and (2) is (2 * (x * 3 + 3 * x + y))
;
; If we do the same derivative mathematically we get:
;
; derivative of: 2x * (3x + y) + y + 3
; is: 2x * 3 + 2 * (3x + y)
; which is equivalent to: 2(3x + 3x + y)
;
; This could be simplified further, of course, but it shows that the function
; worked. The exercise didn't call for a great level of simplification.

; deriv fn from Section 2.3.2
; ---------------------------

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) (make-sum
                         (make-product (multiplier exp)
                                       (deriv (multiplicand exp) var))
                         (make-product (deriv (multiplier exp) var)
                                       (multiplicand exp))))
        (else (display "unknown expression type -- DERIV"))))

;--- Primitives ----

(define variable? symbol?)

; From Section 2.3.2 code
; -----------------------
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; -----------------------

(define sub-expression? list?)

(define (proper-expression? exp)
  (>= (length exp) 3))

(define (last-expression? exp)
  (null? (cdr exp)))

(define operator cadr)

(define next-expression cddr)

(define first-term car)

(define (second-term e)
  (if (last-expression? (next-expression e))
      (car (next-expression e))
      (next-expression e)))

;--------------------

; Returns true only if expression is like: x + y ...
(define (contiguous-sum? e)
  (and (proper-expression? e) (eq? (operator e) '+)))

(define (gather-products e)
  (if (or (last-expression? e) (contiguous-sum? e))
      (list (car e))
      (cons (car e) (cons '* (gather-products (next-expression e))))))

(define (detect lst test next end)
  (cond ((end lst) #f)
        ((test lst) #t)
        (else (detect (next lst) test next end))))

; Returns true if there's a sum anywhere in the expression
(define (sum? e)
  (detect e contiguous-sum? next-expression last-expression?))

(define (addend e)
  (if (product? e)
      (gather-products e) ;return products if they exist
      (first-term e)))

(define (augend e)
  (if (product? e)
      (augend (next-expression e)) ; if expression starts with products,
                                   ; find augend after products.
      (second-term e)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (sub-expression? a1) (sub-expression? a2))
         (append a1 '(+) a2))
        ((sub-expression? a1) (append a1 '(+) (list a2)))
        ((sub-expression? a2) (append (list a1) '(+) a2))
        (else (list a1 '+ a2))))

(define (product? e)
  (and (proper-expression? e) (eq? (operator e) '*)))

(define multiplier first-term)

(define multiplicand second-term)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (and (sub-expression? m1) (not (sum? m1)))
              (and (sub-expression? m2) (not (sum? m2))))
         (append m1 '(*) m2))
        ((and (sub-expression? m1) (not (sum? m1)))
         (append m1 '(*) (if (sub-expression? m2) m2 (list m2))))
        ((and (sub-expression? m2) (not (sum? m2)))
         (append (if (sub-expression? m1) m1 (list m1)) '(*) m2))
        (else (list m1 '* m2))))