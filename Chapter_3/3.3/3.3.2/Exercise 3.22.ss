; Instead of representing a queue as a pair of pointers, we can build
; a queue as a procedure with local state. The local state will consist
; of pointers to the beginning and the end of an ordinary list. Define a
; make-queue procedure and provide implementations of the queue operations
; using this representation.

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! queue is empty"))
            (else
             (set! front-ptr (mcdr front-ptr)))))
    (define (insert-queue! item)
      (let ((new-pair (mcons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))
    (define (print-queue) (display front-ptr))
   (define (dispatch m)
     (cond ((not (number? m)) (insert-queue! m))
           ((= m 0) (empty-queue?))
           ((= m 1) (delete-queue!))
           ((= m 2) (print-queue))))
    dispatch))

(define (empty-queue? q) (q 0))
(define (delete q) (q 1))
(define (print-queue q) (q 2))
(define (insert q item) (q item))