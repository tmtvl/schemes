(import (scheme base) (scheme write))

(define stream
  (lambda (element function)
    (cons element (lambda () (function element)))))

(define tail
  (lambda (stream)
    (tail-no-memoize stream)))

(define tail-no-memoize
  (lambda (stream)
    (let
	((x (cdr stream)))
      (if (procedure? x)
	  (x)
	  x))))

(define tail-memoize
  (lambda (stream)
    (let
	((x (cdr stream)))
      (if (procedure? x)
	  (set-cdr! stream (x)))
      (cdr stream))))

(define show-stream
  (lambda (stream joiner count)
    (display (car stream))
    (cond
     ((and (not (null? (tail stream)))
	   (> count 1))
      (display joiner)
      (show-stream (tail stream) joiner (- count 1)))
     (else (newline)))))

(define upfrom
  (lambda (start)
    (stream
     start
     (lambda (n)
       (upfrom (+ n 1))))))

(show-stream (upfrom 1) ", " 5)
