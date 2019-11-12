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
      (let ((value (it)))
	(if (null? value)
	    value
	    (transform value))))))

(define eachlike
  (lambda (it transform)
    (imap
     it
     (lambda (value)
       (cons value (transform value))))))

(define igrep
  (lambda (it match)
    (let
	((grepper (lambda ()
		    (let ((value (it)))
		      (if (or (null? value) (match value)) value (grepper))))))
      grepper)))

(define list-iterator
  (lambda (l)
    (imap
     (iterator
      l
      (cdr l)
      null?)
     car)))

(define iappend
  (lambda (iterators)
    (let ((next
	   (lambda ()
	     (if (null? iterators)
		 '()
		 (let ((value ((car iterators))))
		   (cond
		    ((null? value)
		     (set! iterators (cdr iterators))
		     (next))
		    (else value)))))))
      next)))
