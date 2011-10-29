; Implement the union-set operation for the unordered-list
; representation of sets.

; Section 2.3.3 code
; ------------------

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set) set (cons x set)))

; -------------------

(define (union-set s1 s2)
  (if (null? s1)
      s2
      (union-set (cdr s1) (adjoin-set (car s1) s2))))