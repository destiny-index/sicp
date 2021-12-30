(import (rnrs))

(define (square x) (* x x))

;; Hash Table

(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

;; Tagging

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

(define (complex? z)
  (eq? (type-tag z) 'complex))

;; Generic Application

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC" (list op type-tags))))))

;; Rectangular Implementation

(define (install-rectangular-package)
  ;; internal procedures

  (define (real-part z)
    (car z))

  (define (imag-part z)
    (cdr z))

  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

  (define (angle z)
    (atan (imag-part z) (real-part z)))

  (define (make-from-real-imag x y)
    (cons x y))

  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system

  (define (tag x)
    (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)

  (put 'imag-part '(rectangular) imag-part)

  (put 'magnitude '(rectangular) magnitude)

  (put 'angle '(rectangular) angle)

  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)

;; Polar Implementation

(define (install-polar-package)
  ;; internal procedures

  (define (magnitude z)
    (car z))

  (define (angle z)
    (cdr z))

  (define (real-part z)
    (* (magnitude z) (cos (angle z))))

  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))

  (define (make-from-mag-ang r a)
    (cons r a))

  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system

  (define (tag x) (attach-tag 'polar x))

  (put 'real-part '(polar) real-part)

  (put 'imag-part '(polar) imag-part)

  (put 'magnitude '(polar) magnitude)

  (put 'angle '(polar) angle)

  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))

  'done)

;; Complex Package

(define (install-complex-package)
  (install-polar-package)
  (install-rectangular-package)

  ;; internal procedures

  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))

  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  (define (real-part z) (apply-generic 'real-part z))

  (define (imag-part z) (apply-generic 'imag-part z))

  (define (magnitude z) (apply-generic 'magnitude z))

  (define (angle z) (apply-generic 'angle z))


  ;; interface to the rest of the system

  (define (tag z)
    (attach-tag 'complex z))

  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))

  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))

  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))

  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))

  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  ;; Tests

  (define epsilon 0.00000001)

  (define (close-enough? a b)
    (cond ((and (pair? a) (pair? b))
           (and (close-enough? (car a) (car b))
                (close-enough? (cdr a) (cdr b))))
          ((and (number? a) (number? b))
           (< (abs (- a b)) epsilon))
          (else (equal? a b))))

  (assert (rectangular? (attach-tag 'rectangular '())))

  (assert (polar? (attach-tag 'polar '())))

  (assert (close-enough? (make-from-real-imag 4 6)
                         (add-complex (make-from-real-imag 1 2)
                                      (make-from-real-imag 3 4))))

  (assert (close-enough? (make-from-mag-ang (* 2 3) (+ 3 4))
                         (mul-complex (make-from-mag-ang 2 3)
                                      (make-from-mag-ang 3 4))))

  'done)

(install-complex-package)
