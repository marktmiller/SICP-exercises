; Section 2.2 code
; ----------------

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Fill in the missing expressions to complete the following definitions of some
; basic list-manipulation operations as accumulations:
;
; (define (map p sequence)
;    (accumulate (lambda (x y) <??>) nil sequence))
; (define (append seq1 seq2)
;    (accumulate cons <??> <??>))
; (define (length sequence)
;    (accumulate <??> 0 sequence))

(define (seqmap p sequence)
  (accumulate (lambda (x y) (p x y)) '() sequence))

(define (seqappend seq1 seq2)
  (accumulate cons seq2 seq1))

(define (seqlength sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
