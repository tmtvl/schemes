(import (scheme base) (scheme cxr) (scheme write))

(define print-line
  (lambda (line)
    (display line)
    (newline)))

(define to-string
  (lambda (value)
    (cond
     ((number? value) (number->string value))
     ((symbol? value) (symbol->string value)))))

(define ast-to-string
  (lambda (tree)
    (if (list? tree)
	(let
	    (
	     (op (to-string (car tree)))
	     (s1 (ast-to-string (cadr tree)))
	     (s2 (ast-to-string (caddr tree)))
	     )
	  (string-append "(" s1 " " op " " s2 ")"))
	(to-string tree))))

(print-line (ast-to-string (list '+ 1 (list '* 2 3))))
