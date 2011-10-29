; Suppose we want to modify the differentiation program so that it
; works with ordinary mathematical notation, in which + and * are
; infix rather than prefix operators. Since the differentiation
; program is defined in terms of abstract data, we can modify it to
; work with different representations of expressions solely by
; changing the predicates, selectors, and constructors that define
; the representation of the algebraic expressions on which the
; differentiator is to operate.
;
; Show how to do this in order to differentiate algebraic expressions
; presented in infix form, such as (x + (3 * (x + (y + 2)))). To
; simplify the task, assume that + and * always take two arguments
; and that expressions are fully parenthesized.

; Section 2.3.2 code
; ------------------

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

(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (augend e)
  (caddr e))

(define (multiplicand e)
  (caddr e))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

; ------------------------------------------------

(define (sum? e)
  (and (pair? e) (eq? (cadr e) '+)))

(define (addend e)
  (car e))

(define (product? e)
  (and (pair? e) (eq? (cadr e) '*)))

(define (multiplier e)
  (car e))