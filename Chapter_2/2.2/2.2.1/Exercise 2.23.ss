; The procedure for-each is similar to map. It takes as arguments
; a procedure and a list of elements. However, rather than forming
; a list of the results, for-each just applies the procedure to
; each of the elements in turn, from left to right. The values
; returned by applying the procedure to the elements are not
; used at all. Give an implementation of for-each.

(define (for-each2 proc lst)
  (cond ((null? lst) #t)
        (else (proc (car lst))
              (for-each2 proc (cdr lst)))))