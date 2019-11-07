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

(define upto
  (lambda (from to)
    (iterator
     from
     (lambda (n) (+ n 1))
     (lambda (n) (> n to)))))

(define five-to-nine (upto 5 9))

(display (list (five-to-nine) (five-to-nine) (five-to-nine) (five-to-nine) (five-to-nine) (five-to-nine)))

(newline)
