(import (scheme base) (scheme write))

(define print-line
  (lambda (line)
    (display line)
    (newline)))

(define fib
  (let ((cache (list (cons 0 1) (cons 1 1))))
    (lambda (n)
      (cond
       ((assoc n cache) (cdr (assoc n cache)))
       (else (set! cache (cons (cons n (+ (fib (- n 1)) (fib (- n 2)))) cache))
	     (cdr (assoc n cache)))))))

(print-line (fib 200))
