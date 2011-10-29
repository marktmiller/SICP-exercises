; A deque ("double-ended queue") is a sequence in which items can be
; inserted and deleted at either the front or the rear. Operations on
; deques are the constructor make-deque, the predicate empty-deque?,
; selectors front-deque and rear-deque, and mutators
; front-insert-deque!, rear-insert-deque!, front-delete-deque!, and
; rear-delete-deque!.
;
; Give implementations of these operations. A condition is
; all operations should be accomplished in O(1) steps.

(define (make-deque)
  (let ((forward-ptr '())
        (reverse-ptr '()))
    (define (empty-deque?) (null? forward-ptr))
    (define (front-deque) (mcar (mcar forward-ptr)))
    (define (rear-deque) (mcar (mcar reverse-ptr)))
    (define (forward-item) (mcar forward-ptr))
    (define (reverse-item) (mcar reverse-ptr))
    (define (front-insert-deque! item)
      (let ((new-node (mcons (mcons item '()) '())))
        (cond ((empty-deque?)
               (set! forward-ptr new-node)
               (set! reverse-ptr new-node))
              (else
               (set-mcdr! (mcar new-node) forward-ptr)
               (set! forward-ptr new-node)))))
    (define (rear-insert-deque! item)
      (let ((new-node (mcons (mcons item '()) reverse-ptr)))
        (cond ((empty-deque?)
               (set! forward-ptr new-node)
               (set! reverse-ptr new-node))
              (else
               (set-mcdr! (reverse-item) new-node)
               (set! reverse-ptr new-node)))))
    (define (front-delete-deque!)
      (cond ((empty-deque?) (error "FRONT DELETE! empty queue"))
            ((equal? forward-ptr reverse-ptr)
             (set! forward-ptr '())
             (set! reverse-ptr '()))
            (else
             (set! forward-ptr (mcdr (forward-item)))
             (set-mcdr! forward-ptr '()))))
    (define (rear-delete-deque!)
      (cond ((empty-deque?) (error "REAR DELETE! empty queue"))
            ((equal? forward-ptr reverse-ptr)
             (set! forward-ptr '())
             (set! reverse-ptr '()))
            (else
             (set! reverse-ptr (mcdr reverse-ptr))
             (set-mcdr! (reverse-item) '()))))
    (define (print-list item-ptr)
      (cond ((null? item-ptr) '())
            (else
             (display (mcar (mcar item-ptr)))
             (print-list (mcdr (mcar item-ptr))))))
    (define (print-deque)
      (define item-ptr forward-ptr)
      (display "(")
      (cond ((empty-deque?) '())
            (else (print-list item-ptr)))
      (display ")"))
    (define (dispatch idx obj)
      (cond ((= idx 0) (empty-deque?))
            ((= idx 1) (front-deque))
            ((= idx 2) (rear-deque))
            ((= idx 3) (front-insert-deque! obj))
            ((= idx 4) (rear-insert-deque! obj))
            ((= idx 5) (front-delete-deque!))
            ((= idx 6) (rear-delete-deque!))
            ((= idx 7) (print-deque))))
    dispatch))

(define (empty-deque? a) (a 0 '()))
(define (front-deque a) (a 1 '()))
(define (rear-deque a) (a 2 '()))
(define (front-insert-deque! a obj) (a 3 obj))
(define (rear-insert-deque! a obj) (a 4 obj))
(define (front-delete-deque! a) (a 5 '()))
(define (rear-delete-deque! a) (a 6 '()))
(define (print-deque a) (a 7 '()))