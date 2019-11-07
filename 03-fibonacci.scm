(import (scheme base) (scheme write))

(define fibonacci
  (lambda (n)
    (fibonacci-memoization n)))

(define fibonacci-no-memoization
  (lambda (n)
    (if (< n 2)
	1
	(+
	 (fibonacci-no-memoization (- n 1))
	 (fibonacci-no-memoization (- n 2))))))

(define fibonacci-memoization
  (lambda (n)
    (fibonacci-step n 1 1)))

(define fibonacci-step
  (lambda (n a b)
    (if (< n 2)
	b
	(fibonacci-step
	 (- n 1)
	 b
	 (+ a b)))))

(define print-line
  (lambda (n)
    (display n)
    (newline)))

(print-line (fibonacci 89))
