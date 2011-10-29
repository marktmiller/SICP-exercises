; Suppose we change the representation of mobiles so that the
; constructors are

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; How much do you need to change your programs to convert to the
; new representation?

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

(define (left-branch tree)
  (car tree))

; Changed from (cdar tree)
(define (right-branch tree)
  (cdr tree))

; (length (length structure))
(define (branch-length branch)
  (car branch))

; Changed from (cdar branch)
(define (branch-structure branch)
  (cdr branch))

(define (total-weight node)
  (cond ((number? node) node)
        (else (+ (total-weight (branch-structure (left-branch node)))
                 (total-weight (branch-structure (right-branch node)))))))

(define (torque node)
  (* (branch-length node) (total-weight (branch-structure node))))

; Changed from (list? ...)
(define (pred-procedure node)
  (if (pair? (branch-structure node))
      (mobile-balanced? (branch-structure node))
      #t))

(define (mobile-balanced? node)
  (define left (left-branch node))
  (define right (right-branch node))
  (if (= (torque left) (torque right))
       (and (pred-procedure left) (pred-procedure right)) #f))