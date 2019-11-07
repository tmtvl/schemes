(import (scheme base) (scheme write))

(define stack (cons '() '()))

(define pop
  (lambda (list)
    (cond
     ((null? (cdr list))
      (let ((r (car list)))
	(set-car! list '())
	r))
     ((null? (cddr list))
      (let ((r (cadr list)))
	(set-cdr! list '())
	r))
     (else (pop (cdr list))))))

(define push
  (lambda (list value)
    (cond
     ((null? (car list))
      (set-car! list value))
     ((null? (cdr list))
      (set-cdr! list (cons value '())))
     (else (push (cdr list) value)))))

(define dispatch-table
  (list
   (cons
    '+
    (lambda ()
      (push stack (+ (pop stack) (pop stack)))))
   (cons
    '-
    (lambda ()
      (push stack (- (pop stack) (pop stack)))))
   (cons
    '*
    (lambda ()
      (push stack (* (pop stack) (pop stack)))))
   (cons
    '/
    (lambda ()
      (push stack (/ (pop stack) (pop stack)))))))

(define parse-list
  (lambda (list)
    (for-each
     (lambda (value)
       (display "stack: ")
       (print-line stack)
       (display "current value: ")
       (print-line value)
       (cond
	((not (assoc value dispatch-table))
	 (if (number? value)
	     (push stack value)
	     (error "unrecognizable value" value)))
	(else
	 ((cdr (assoc value dispatch-table))))))
     list)
    (pop stack)))

(define print-line
  (lambda (a)
    (display a)
    (newline)))

(print-line (parse-list (list 1 2 '+ 4 '*)))
(print-line (parse-list (list 2 3 4 '* '+)))
