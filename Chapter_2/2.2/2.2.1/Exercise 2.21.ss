; The procedure square-list takes a list of numbers as argument and
; returns a list of the square of those numbers.
;
; Here are two different definitions of square-list. Complete both
; of them by filling in the missing expressions:
;
; (define (square-list items)
;    (if (null? items)
;       '()
;       (cons <??> <??>)))
; (define (square-list items)
;    (map <??> <??>))

(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))