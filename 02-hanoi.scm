(import (scheme base) (scheme write))

(define move-disc
  (lambda (disc from to)
    (display "Move disc ")
    (display disc)
    (display " from ")
    (display from)
    (display " to ")
    (display to)
    (newline)))

(define hanoi
  (lambda (discs start goal extra)
    (cond
     ((null? (cdr discs)) (move-disc (car discs) start goal))
     (else
      (hanoi (cdr discs) start extra goal)
      (move-disc (car discs) start goal)
      (hanoi (cdr discs) extra goal start)))))

(hanoi (list 3 2 1) #\a #\c #\b)
