;;; Solution to SICP Exercise 2.42 - The eight-queens puzzle

(library (eight-queens)
  (export queens)
  (import (rnrs))

  (define (queens board-size)
    (define (queen-cols k)
      (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map
                (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
    (queen-cols board-size))

  (define empty-board '())

  (define (adjoin-position row col rest-of-queens)
    (cons (cons row col) rest-of-queens))

  (define (safe? _ positions)
    (let ((new-queen (car positions))
          (rest-of-queens (cdr positions)))
      (every?
        (map
          (lambda (old-queen) (not (threatens? new-queen old-queen)))
          rest-of-queens))))

  (define (threatens? new-queen old-queen)
    (let ((vertical-distance (abs (- (car new-queen) (car old-queen))))
          (horizontal-distance (abs (- (cdr new-queen) (cdr old-queen)))))
      (or (= vertical-distance 0)
          (= vertical-distance horizontal-distance))))


  ;; Other List Procedures

  (define (enumerate-interval low high)
    (if (> low high)
      '()
      (cons low (enumerate-interval (+ 1 low) high))))

  (define (every? sequence)
    (accumulate (lambda (val acc) (and val acc)) #t sequence))

  (define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

  (define (accumulate op initial sequence)
    (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

  (display "Found ")
  (display (length (queens 8)))
  (display " of 92 solutions.")
  (newline)
)
