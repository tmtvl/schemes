(import (scheme base) (scheme write))

(define to-binary
  (lambda (number)
    (if (< number 2)
	(number->string number)
	(let ((x (truncate-remainder number 2)) (y (truncate (/ number 2))))
	  (string-append (to-binary y) (number->string x))))))

(define print-line
  (lambda (x)
    (display x)
    (newline)))

(define print-binary-conversion
  (lambda (number)
    (print-line (string-append (number->string number) ": " (to-binary number)))))

(print-binary-conversion 3)
(print-binary-conversion 12)
(print-binary-conversion 60)
