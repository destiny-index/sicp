(import (rnrs))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(assert (element-of-set? 'x '(x y z)))
(assert (not (element-of-set? 'x '(y z))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(assert (equal? '(x y) (adjoin-set 'x '(y))))
(assert (equal? '(x y) (adjoin-set 'x '(x y))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
         '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(assert (equal? '(b c) (intersection-set '(a b c) '(b c d))))
