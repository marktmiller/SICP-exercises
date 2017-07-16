; One variant of the Fermat test that cannot be fooled is called the
; Miller-Rabin test. This starts from an alternate form of Fermat's Little
; Theorem, which states that if n is a prime number and a is any positive
; integer less than n, then a rasied to the (n - 1)st power is congruent to
; 1 modulo n. To test the primality of a number n by the Miller-Rabin test,
; we pick a random number a < n and raise a to the (n - 1)st power modulo n
; using the expmod procedure. However, whenever we perform the squaring step
; in expmod, we check to see if we have discovered a "nontrivial square root
; of 1 modulo n," that is, a number not equal to 1 or n - 1 whose square is
; equal to 1 modulo n. It is possible to prove that if such a nontrivial
; root of 1 exists, then n is not prime. It is also possible to prove that if
; n is an odd number that is not prime, then, for at least half the numbers
; a < n, computing a^(n - 1) in this way will reveal a nontrivial square
; root of 1 modulo n. (This is why the Miller-Rabin test cannot be fooled.)
; Modify the expmod procedure to signal if it discovers a nontrivial square
; root of 1, and use this to implement the Miller-Rabin test with a procedure
; analogous to fermat-test. Check your procedure by testing various known
; primes and non-primes. Hint: One convenient way to make expmod signal is to
; have it return 0.

(define (square n)
  (pow n 2))

(define (expmod b i d r n)
  (define (first-case)
    (define root (remainder (pow b d) n))
    (if (or (= root 1) (= root (- n 1)))
        b
        (expmod (remainder (square root) n) 1 d r n)))
  (cond ((= i 0) (first-case))
        ((or (> i (- r 1)) (= b 1)) 0)
        ((= b (- n 1)) b)
        (else (expmod (remainder (square b) n) (+ i 1) d r n))))             

(define (odd-factor n)
  (cond ((odd? n) n)
        (else (odd-factor (/ n 2)))))

(define (pow x n)
  (cond ((= n 0) 1)
        ((= n 1) x)
        (else (* x (pow x (- n 1))))))

(define (constrain-range n l h)
  (cond ((<= n l) (+ l 1))
        ((>= n h) (- h 1))
        (else n)))

(define (test-prime n d r)
  (> (expmod (constrain-range (random (- n 1)) 1 (- n 1)) 0 d r n) 0))

(define (iterate-prime n d r times)
  (cond ((= times 0) '#t)
        ((test-prime n d r) (iterate-prime n d r (- times 1)))
        (else '#f)))

(define (start-prime-test n times)
  (define d (odd-factor (- n 1)))
  (define r (/ (- n 1) (* d 2)))
  (cond ((= n 0) (display "Error: n cannot be zero"))
        ((< n 4) '#t)
        ((even? n) '#f)
        (else (iterate-prime n d r times))))
