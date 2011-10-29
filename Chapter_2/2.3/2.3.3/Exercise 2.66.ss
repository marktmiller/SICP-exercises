; Implement a lookup procedure where a set of records is structured as a
; binary tree, ordered by the numerical values of the keys.

; Section 2.3.3 code
; ------------------

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                               right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                       remaining-elts))))))))

(define (make-tree entry left right)
  (list entry left right))

(define (make-entry key value)
  (list key value))

(define entry car)

(define left cadr)

(define right caddr)

(define key car)

(define value cadr)

; ----------------------

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right set-of-records)))))