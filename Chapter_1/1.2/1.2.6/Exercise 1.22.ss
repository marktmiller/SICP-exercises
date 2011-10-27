; Using timed-prime-test, write a procedure search-for-primes that checks
; the primality of consecutive odd integers in a specified range. Use your
; procedure to find the three smallest primes larger than 1000; larger
; than 10,000; larger than 100,000; larger than 1,000,000. Note the time
; needed to test each prime.

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square a)
  (* a a))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-milliseconds) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display (if (< elapsed-time 0) (- elapsed-time) elapsed-time))
  #t)

(define (search-for-primes value primes-found)
  (define primes
    (+ primes-found (if (and (< primes-found 3) (timed-prime-test value)) 1 0)))
  (if (< primes-found 3)
      (search-for-primes (+ value (if (even? value) 1 2)) primes) '()))