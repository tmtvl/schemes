(import (scheme base) (scheme write))

(define cache '())

(define rgb-to-cmyk
  (lambda (red green blue)
    (if (and (not (null? cache)) (assoc (list red green blue) cache))
	(cdr (assoc (list red green blue) cache))
	(let* ((cyan (- 255 red))
		  (magenta (- 255 green))
		  (yellow (- 255 blue))
		  (key (min cyan magenta yellow)))
	  (print-line (string-append "calculating for values: " (number->string red) ", " (number->string green) ", " (number->string blue)))
	  (set! cyan (- cyan key))
	  (set! magenta (- magenta key))
	  (set! yellow (- yellow key))
	  (set! cache (cons
		       (cons (list red green blue) (list cyan magenta yellow key))
		       cache))
	  (list cyan magenta yellow key)))))

(define print-line
  (lambda (line)
    (display line)
    (newline)))

(print-line (rgb-to-cmyk 191 31 7))
(print-line (rgb-to-cmyk 15 63 255))
(print-line (rgb-to-cmyk 191 31 7))
