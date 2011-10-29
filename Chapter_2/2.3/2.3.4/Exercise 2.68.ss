; Write a procedure, encode-symbol, that returns the list of bits that
; encodes a given symbol according to a given Huffman tree.

; Section 2.3.4 code
; ------------------

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (display "bad bit -- CHOOSE-BRANCH"))))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; ------------------------

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

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