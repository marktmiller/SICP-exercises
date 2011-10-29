; Abstract your answer from exercise 2.30 to produce a tree-map
; with the property that square-tree could be defined as
;
; (define (square-tree tree) (tree-map square tree))

(define (tree-map fn tree)
  (map (lambda (x) (if (number? x) (fn x) (tree-map fn x))) tree))

(define (square-tree tree)
  (tree-map square tree))

(define (square x)
  (* x x))