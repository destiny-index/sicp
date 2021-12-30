(import (rnrs))

(define (puts m)
  (display m)
  (newline))

(define (make-account balance)

  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))

  dispatch)

(define acc (make-account 100))
(puts ((acc 'withdraw) 50))
(puts ((acc 'withdraw) 60))
(puts ((acc 'deposit) 40))
(puts ((acc 'withdraw) 60))
