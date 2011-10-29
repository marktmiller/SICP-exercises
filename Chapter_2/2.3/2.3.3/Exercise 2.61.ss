; Give an implementation of adjoin-set using the ordered (list)
; representation. By analogy with element-of-set? show how to
; take advantage of the ordering to produce a procedure that
; requires on the average about half as many steps as with
; the unordered representation.

(define (find-value x s1 s2 next)
  (cond ((or (and (not (null? s1)) (= x (car s1)))
             (and (not (null? s2)) (= x (car s2)))) #t)
        ((and (or (null? s1) (< x (car s1)))
              (or (null? s2) (< x (car s2)))) #f)
        (else (find-value x (next s1) (next s2) next))))

(define (element-of-set? x set)
  (find-value x set (cdr set)
              (lambda (s)
                (if (and (not (null? s)) (not (null? (cdr s)))
                         (not (null? (cddr s))))
                    (cddr s)
                    '()))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((element-of-set? x set) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))