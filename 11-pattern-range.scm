(import (scheme base) (scheme write))

(define iterator
  (lambda (value next done)
    (lambda ()
      (cond
       ((done value) '())
       (else
	(let ((current value))
	  (set! value (next value))
	  current))))))

(define imap
  (lambda (it transform)
    (lambda ()
      (transform (it)))))

(define factorial
  (lambda (n)
    (if (= n 1)
	1
	(* n (factorial (- n 1))))))

(define number-to-pattern
  (lambda (n length lim)
    (cond
     ((> length 0)
      (let ((f (/ lim length)))
	(cons
	 (truncate/ n f)
	 (number-to-pattern
	  (truncate-remainder n f)
	  (- length 1)
	  f))))
     (else '()))))

(define pattern-iterator
  (lambda (length)
    (let* ((lim (factorial length))
	  (joiner (lambda (n) (cons n (number-to-pattern n length lim)))))
      (imap
       (iterator (joiner 0)
		 (lambda (x) (joiner (+ (car x) 1)))
		 (lambda (x) (= (car x) lim)))
       (lambda (x) (if (null? x) x (cdr x)))))))

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

(define pattern-of-six (pattern-iterator 6))

(show-all-patterns pattern-of-six)
