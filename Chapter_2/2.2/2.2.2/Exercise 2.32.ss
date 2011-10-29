; We can represent a set as a list of distinct elements, and we can
; represent the set of all subsets of the set as a list of lists.
; For example, if the set is (1 2 3), then the set of all subsets
; is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the
; following definition of a procedure that generates the set of
; subsets of a set and give a clear explanation of why it works:
;
; (define (subsets s)
;    (if (null? s)
;       (list '())
;       (let ((rest (subsets (cdr s))))
;          (append rest (map <??> rest)))))

; How it works:
;
; The function starts off going to the end of the input list.
; It then constructs the subsets as it backs out of recursion.
; It's similar to the solution for Exercise 1.11 in the sense
; that the formulation of the result gets built up as the
; function runs, in intermediate steps. The set of subsets for
; step n-1 in the process is based on, or constructed from,
; the set of subsets for step n.
;
; Using the example of (1 2 3) from the book, here is a walkthrough of
; the execution:
;
; (subsets (1 2 3))
; (subsets (2 3))
; (subsets (3))
; (subsets ())
;    returns (())
;
; back to (subsets (3))
;    The result is (() [from appending "rest", and]
;       (3) [from (map (lambda (x) (append (3)[car of s] x)) (()))]
;    returns (() (3))
;
; back to (subsets (2 3))
;    The result is (() (3) [and] (2) (2 3)
;      [from (map (lambda (x) (append (2)[car of s] x)) (() (3))))
;    returns (() (3) (2) (2 3))
;
; back to (subsets (1 2 3))
;    The result is (() (3) (2) (2 3) [and] (1) (1 3) (1 2) (1 2 3))
;       [from (map (lambda (x)
;                     (append (1)[car of s] x)) (() (3) (2) (2 3)))]
;    returns (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;
; This really shows the effective use of the stack for saving state.

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append (list (car s)) x)) rest)))))