; Write an O(n) implementation of union-set and intersection-set
; for sets implemented as balanced binary trees.

; Section 2.3.3 code
; ------------------

(define (make-tree entry left right) (list entry left right))

(define entry car)

(define left-branch cadr)

(define right-branch caddr)

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

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

; Defined as intersection-set in Sec. 2.3.3
(define (intersection-list l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (let ((x1 (car l1)) (x2 (car l2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-list (cdr l1)
                                        (cdr l2))))
              ((< x1 x2)
               (intersection-list (cdr l1) l2))
              ((< x2 x1)
               (intersection-list l1 (cdr l2)))))))

; --------------------------

(define (union-list l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((> (car l1) (car l2)) (cons (car l2) (union-list l1 (cdr l2))))
        ((< (car l1) (car l2)) (cons (car l1) (union-list (cdr l1) l2)))
        ((= (car l1) (car l2)) (cons (car l1) (union-list (cdr l1)
                                                          (cdr l2))))))

(define (union-set s1 s2)
  (list->tree (union-list (tree->list-1 s1) (tree->list-1 s2))))

(define (intersection-set s1 s2)
  (list->tree (intersection-list (tree->list-1 s1) (tree->list-1 s2))))