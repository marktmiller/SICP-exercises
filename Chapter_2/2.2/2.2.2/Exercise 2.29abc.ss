; A binary mobile consists of two branches, a left branch and a right
; branch. Each branch is a rod of a certain length, from which hangs
; either a weight or another binary mobile. We can represent a
; binary mobile using compound data by constructing it from two
; branches (for example, using list):

(define (make-mobile left right)
  (list left right))

; A branch is constructed from a length (which must be a number)
; together with a structure, which may be either a number
; (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

; a. Write the corresponding selectors left-branch and right-branch,
;    which return the branches of a mobile, and branch-length and
;    branch-structure, which return the components of a branch.
;
; b. Using your selectors, define a procedure total-weight that
;    returns the total weight of a mobile.
;
; c. A mobile is said to be balanced if the torque applied by its
;    top-left branch is equal to that applied by its top-right
;    branch (that is, if the length of the left rod multiplied by
;    the weight hanging from that rod is equal to the corresponding
;    product for the right side) and if each of the submobiles
;    hanging off its branches is balanced. Design a predicate that
;    tests whether a binary mobile is balanced.

; ((left-branch) (right-branch))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

; (length (length structure))
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight node)
  (cond ((number? node) node)
        (else (+ (total-weight (branch-structure (left-branch node)))
                 (total-weight (branch-structure (right-branch node)))))))

(define (torque node)
  (* (branch-length node) (total-weight (branch-structure node))))

(define (pred-procedure node)
  (if (list? (branch-structure node))
      (mobile-balanced? (branch-structure node))
      #t))

(define (mobile-balanced? node)
  (define left (left-branch node))
  (define right (right-branch node))
  (if (= (torque left) (torque right))
       (and (pred-procedure left) (pred-procedure right)) #f))

(define tree (make-mobile
                (make-branch 16
                 (make-mobile
                  (make-branch 8
                   (make-mobile (make-branch 3 8) (make-branch 3 8)))
                  (make-branch 8
                   (make-mobile (make-branch 5 8) (make-branch 5 8)))))
                (make-branch 16
                             (make-mobile
                              (make-branch 8
                                           (make-mobile
                                            (make-branch 8 8)
                                            (make-branch 8 8)))
                              (make-branch 8
                                           (make-mobile
                                            (make-branch 10 8)
                                            (make-branch 10 8)))))))