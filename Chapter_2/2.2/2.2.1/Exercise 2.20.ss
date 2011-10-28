; The procedures +, *, and list take arbitrary numbers of arguments.
; One way to define such procedures is to use define with dotted-tail
; notation. Use this notation to write a procedure same-parity that
; takes one or more integers and returns a list of all the
; arguments that have the same even-odd parity as the first argument.
; For example,
;
; > (same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)
;
; > (same-parity 2 3 4 5 6 7)
; (2 4 6)

(define (select lst test)
  (cond ((null? lst) '())
        ((test (car lst)) (append (list (car lst))
                                  (select (cdr lst) test)))
        (else (select (cdr lst) test))))

(define (same-parity . l)
  (select l (if (even? (car l)) even? odd?)))