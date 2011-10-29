; Generalizing one- and two-dimensional tables, show how to implement
; a table in which values are stored under an arbitrary number of keys
; and different values may be stored under different numbers of keys.
; The lookup and insert! procedures should take as input a list of
; keys used to access the table.
;
; This description was kind of confusing, but I took it to mean that
; I should strictly generalize one- and two-dimensional tables,
; which just means not forcing the list of keys to be length 1 or 2.

(define (make-table)
  (let ((local-table (mcons '*table* '())))
    (define (extend-list rec lst) (mcons rec lst))
    (define (create-master key data secondary-list)
      (mcons key (mcons data secondary-list)))
    (define (create-leaf key data) (mcons key data))
    (define (first-key keys) (car keys))
    (define (second-key keys) (cadr keys))
    (define (second-key? keys) (not (null? (cdr keys))))
    (define (insert! keys value)
      (let ((record1 (assoc (car keys) (mcdr local-table))))
        (cond ((null? record1)
               (set-mcdr! local-table
                          (extend-list
                           (create-master
                            (first-key keys)
                            (if (second-key? keys) '() value)
                            (if (second-key? keys)
                                (extend-list
                                 (create-leaf (second-key keys)
                                              value)
                                 '())
                                '()))
                           (mcdr local-table))))
              ((not (second-key? keys)) (set-mcar! (mcdr record1) value))
              (else
               (let ((record2 (assoc (second-key keys)
                                     (mcdr (mcdr record1)))))
                 (cond ((null? record2)
                          (set-mcdr! (mcdr record1)
                                     (extend-list
                                      (create-leaf (second-key keys)
                                                   value)
                                      (mcdr (mcdr record1)))))
                       (else (set-mcdr! record2 value))))))))
    (define (lookup keys)
      (let ((record1 (assoc (first-key keys) (mcdr local-table))))
        (cond ((null? record1) '())
              ((not (second-key? keys)) (mcar (mcdr record1)))
              (else
               (let ((record2 (assoc (second-key keys)
                                     (mcdr (mcdr record1)))))
                 (if (null? record2) '() (mcdr record2)))))))
    (define (assoc key lst)
      (cond ((null? lst) '())
            ((equal? (mcar (mcar lst)) key) (mcar lst))
            (else (assoc key (mcdr lst)))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (insert! obj key value) ((obj 'insert-proc!) key value))
(define (lookup obj key) ((obj 'lookup-proc) key))