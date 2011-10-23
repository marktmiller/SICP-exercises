; Write a procedure that computes elements of Pascal's triangle by means of
; a recursive process.

(define (pascal row col)
   (cond ((or (< row 1) (> col row)) -1)
              ((or (= row 1) (= col 1) (= row col)) 1)
              (else (+ (pascal (- row 1) (- col 1))
                       (if (< col (- row 1)) (pascal (- row 1) col) 1)))))
