(import (scheme base) (scheme write))

(define tail
  (lambda (stream)
    (let ((t (cdr stream)))
      (if (procedure? t)
	  (set-cdr! stream (t)))
      (cdr stream))))

(define transform
  (lambda (stream f)
    (cond
     ((null? stream) '())
     (else
      (cons
       (f (car stream))
       (lambda ()
	 (transform (tail stream) f)))))))

(define grep
  (lambda (stream match)
    (cond
     ((null? stream) '())
     (else
      (if (match (car stream))
	  (cons
	   (car stream)
	   (lambda ()
	     (grep (tail stream) match)))
	  (grep (tail stream) match))))))

(define merge
  (lambda (a b before)
    (cond
     ((null? a) b)
     ((null? b) a)
     (else
      (let ((x (car a)) (y (car b)))
	(cond
	 ((before x y)
	  (cons
	   x
	   (lambda ()
	     (merge (tail a) b before))))
	 ((before y x)
	  (cons
	   y
	   (lambda ()
	     (merge a (tail b) before))))
	 (else
	  (cons
	   x
	   (lambda ()
	     (merge (tail a) (tail b) before))))))))))

(define scale
  (lambda (number-stream exponent)
    (transform number-stream (lambda (n) (* n exponent)))))

(define merge-less
  (lambda (a b)
    (merge a b <)))

(define hamming
  (cons
   1
   (lambda ()
     (merge-less
      (scale hamming 2)
      (merge-less
       (scale hamming 3)
       (scale hamming 5))))))

(define show-count
  (lambda (stream count)
    (display (car stream))
    (cond
     ((> count 1)
      (display #\space)
      (show-count (tail stream) (- count 1)))
     (else
      (newline)))))

(define union
  (lambda (streams)
    (cond
     ((null? streams) '())
     (else
      (let ((h (car streams)))
	(cond
	 ((null? h) (union (cdr streams)))
	 (else
	  (cons
	   (car h)
	   (lambda ()
	     (union (append (cdr streams) (cons (tail h) '()))))))))))))

(define constant-index
  (lambda (v n)
    (cons
     (cons v n)
     (lambda ()
       (constant-index v (+ n 1))))))

(define fish (constant-index "fish" 0))

(show-count hamming 20)
(newline)

(show-count fish 3)
(newline)

(define soup
  (union
   (list
    fish
    (constant-index "dog" 0)
    (constant-index "carrot" 0))))

(show-count soup 8)
