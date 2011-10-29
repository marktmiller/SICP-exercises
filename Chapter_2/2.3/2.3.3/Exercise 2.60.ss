; Suppose we allow duplicates in a set. For instance, the set
; {1,2,3} could be represented as the list (2 3 2 1 3 2 2).
; Design procedures element-of-set?, adjoin-set, union-set, and
; intersection-set that operate on this representation.

; Section 2.3.3 code
; ------------------

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; -------------------

(define (adjoin-set x set)
  (cons x set))

(define (union-set s1 s2)
  (append s1 s2))

(define (intersection-set s1 s2)
  (cond ((null? s1) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))
