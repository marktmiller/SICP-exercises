; Show how to extend the basic differentiator to handle more kinds
; of expressions. For instance, implement the differentiation rule:
; d(u^n)/dx = nu^(n-1) * (du/dx) by adding a new clause to the deriv
; program and defining appropriate procedures exponentiation?, base,
; exponent, and make-exponentiation. Build in the rules that anything
; raised to the power 0 is 1 and anything raised to the power of 1
; is the thing itself.
;
; I used '^' to denote exponentiation.

; Section 2.3.2 code
; ------------------
(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;---------------------------------------------------

(define (match-operator? exp op)
  (and (pair? exp) (eq? (car exp) op)))

(define (first-term exp)
  (cadr exp))

(define (rest exp)
  (caddr exp))

(define (sum? e)
  (match-operator? e '+))

(define (addend e)
  (first-term e))

(define (augend e)
  (rest e))

(define (product? e)
  (match-operator? e '*))

(define (multiplier e)
  (first-term e))

(define (multiplicand e)
  (rest e))

(define (exponentiation? exp)
  (match-operator? exp '^))

(define (base exp)
  (first-term exp))

(define (exponent exp)
  (rest exp))

(define (make-exponentiation base exponent)
  (define (form-exponent)
    (list '^ base exponent))
  
  (cond ((number? base) 0)
        ((number? exponent)
         (cond ((= exponent 1) base)
               ((= exponent 0) 1)
               (else (form-exponent))))
        (else (form-exponent))))

(define (subtraction? exp)
  (match-operator? exp '-))

(define (subend exp)
  (first-term exp))

; Use augend to get 2nd term in subtraction

(define (make-subtraction s1 s2)
  (cond ((=number? s1 0) (- s2))
        ((=number? s2 0) s1)
        ((and (number? s1) (number? s2)) (- s1 s2))
        (else (list '- s1 s2))))

(define (decrement-expression exp)
  (cond ((number? exp) (- exp 1))
        ((variable? exp) (make-subtraction exp 1))
        ((sum? exp)
         (make-addition
          (first-term exp)
          (make-subtraction (rest exp) 1)))
        ((subtraction? exp)
         (make-subtraction
          (first-term exp)
          (make-sum (rest exp) 1)))
        ((or (product? exp) (division? exp)) (make-subtraction exp 1))
        (else
         (display "unknown expression type -- DECREMENT-EXPRESSION"))))

(define (division? exp)
  (match-operator? exp '/))

(define (dividend exp)
  (first-term exp))

(define (divisor exp)
  (rest exp))

(define (make-quotient q1 q2)
  (cond ((=number? q1 0) 0)
        ((=number? q2 0)
         (display "cannot divide by zero -- MAKE-QUOTIENT"))
        ((and (number? q1) (number? q2)) (/ q1 q2))
        (else (list '/ q1 q2))))

;--------------- main derivation fn ---------------

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum
                     (deriv (addend exp) var) (deriv (augend exp) var)))
        ((product? exp) (make-sum
                         (make-product (multiplier exp)
                                       (deriv (multiplicand exp) var))
                         (make-product (deriv (multiplier exp) var)
                                       (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (exponent exp)
          (make-product
           (make-exponentiation
            (base exp)
            (decrement-expression (exponent exp)))
           (deriv (base exp) var))))
        ((subtraction? exp)
         (make-subtraction (deriv (subend exp) var)
                           (deriv (augend exp) var)))
        ((division? exp)
         (make-quotient
          (make-subtraction
           (make-product (divisor exp) (deriv (dividend exp) var))
           (make-product (deriv (divisor exp) var) (dividend exp)))
          (make-exponentiation (divisor exp) 2)))
        (else (display "unknown expression type -- DERIV"))))