; Must be run in Lazy Scheme

; Complete the following definition, which generalizes stream-map to
; procedures that take multiple arguments:
;
; (define (stream-map proc . argstreams)
;    (if (<??> (car argstreams))
;       the-empty-stream
;       (<??>
;          (apply proc (map <??> argstreams))
;          (apply stream-map
;                 (cons proc (map <??> argstreams))))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      '()
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))