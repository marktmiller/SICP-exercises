; Use make-leaf-set (given in Section 2.3.4) to transform the list of
; symbol-frequency pairs into an ordered set of leaves. Write a procedure,
; successive-merge, using procedure make-code-tree (also given in 2.3.4),
; to successively merge the smallest-weight elements of the set until
; there is only one element left, which is the desired Huffman tree.

; Section 2.3.4 code
; ------------------

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

; Takes a list of symbol-frequency pairs such as
; ((A 4) (B 2) (C 1) (D 1))
; and constructs an initial ordered set of leaves ready to be merged
; according to the Huffman algorithm.
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

; ----- exercise code -----

(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (successive-merge (cons
                         (make-code-tree (cadr set) (car set))
                         (if (null? (cddr set)) '() (cddr set))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; -------------------------

; ----- test code -----

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

; Code I wrote from Exercise 2.68
; Each time it goes down a left branch it adds a 0. Each time it goes
; down a right branch it adds 1.
(define (encode-symbol symbol tree)
  (if (element-of-set? symbol (symbols tree))
      (if (not (leaf? tree))
          (if (element-of-set? symbol (symbols (left-branch tree)))
              (cons 0 (encode-symbol symbol (left-branch tree)))
              (cons 1 (encode-symbol symbol (right-branch tree))))
          '())
      (display "Symbol not in tree -- ENCODE-SYMBOL")))

; Code from Section 2.3.3
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

; Code from Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))