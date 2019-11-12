(import (scheme base) (scheme write))

(define factorial
  (lambda (n)
    (if (= n 1)
	1
	(* n (factorial (- n 1))))))

(define number-to-pattern
  (lambda (number length)
    (cond
     ((> length 0) (cons (truncate-remainder number length) (number-to-pattern (truncate (/ number length)) (- length 1))))
     (else '()))))

(define pattern-iterator
  (lambda (length)
    (let ((lim (factorial length))
	  (n 0))
      (lambda ()
	(cond
	 ((< n lim)
	  (let ((pattern (number-to-pattern n length)))
	    (set! n (+ n 1))
	    pattern))
	 (else '()))))))

(define show-pattern
  (lambda (pattern)
    (display pattern)
    (newline)))

(define show-all-patterns
  (lambda (pattern-it)
    (let ((pattern (pattern-it)))
      (cond
       ((not (null? pattern))
	(show-pattern pattern)(show-all-patterns pattern-it))))))

(define pattern-of-one (pattern-iterator 1))

(show-pattern (pattern-of-one))
(show-pattern (pattern-of-one))

(define pattern-of-four (pattern-iterator 4))

(show-all-patterns pattern-of-four)
