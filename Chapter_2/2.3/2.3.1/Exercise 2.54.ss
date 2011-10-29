; Two lists are said to be equal? if they contain equal elements
; arranged in the same order. To be more precise, we can define
; equal? recursively in terms of the basic eq? equality of symbols
; by saying that a and b are equal? if they are both symbols and
; the symbols are eq?, or if they are both lists such that (car a)
; is equal? to (car b) and (cdr a) is equal? to (cdr b). Using this
; idea, implement equal? as a procedure.

(define (my-equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((or (and (symbol? a) (pair? b)) (and (pair? a) (symbol? b))) #f)
        ((and (pair? a) (pair? b) (not (= (length a) (length b)))) #f)
        ((and (symbol? a) (symbol? b)) (eq? a b))
        ((and (pair? a) (pair? b)) (and (my-equal? (car a) (car b))
                                        (my-equal? (cdr a) (cdr b))))))