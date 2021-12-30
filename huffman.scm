(import (rnrs))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; Checks
(define test-leaf-a (list 'leaf 'A 8))
(define test-leaf-b (list 'leaf 'B 3))
(define test-leaf-c (list 'leaf 'C 1))
(define test-tree (list test-leaf-a test-leaf-b (list 'A 'B) 11))
(define test-set (list test-leaf-c test-leaf-a))

; Should determine that test-leaf-a is a leaf
(assert (leaf? test-leaf-a))

; Should get the symbol of the tree
(assert (equal? (list 'A 'B) (symbols test-tree)))

; Should get the weight of the tree
(assert (equal? 11 (weight test-tree)))

; Should construct a tree from two leaves
(assert (equal? test-tree (make-code-tree test-leaf-a test-leaf-b)))

; Should select a branch from the tree
(assert (equal? test-leaf-a (choose-branch 0 test-tree)))
(assert (equal? test-leaf-b (choose-branch 1 test-tree)))

; Should decode using a tree
(assert (equal? (list 'A 'B) (decode (list 0 1) test-tree)))

; Should add an item to the ordered set
(assert (equal? (list test-leaf-c test-leaf-b test-leaf-a)
                (adjoin-set test-leaf-b test-set)))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair))
                  (make-leaf-set (cdr pairs))))))

(assert (equal? test-set
                (make-leaf-set (list '(A 8) '(C 1)))))
