(import (scheme base) (scheme write))

;; (A B C D) -> (A B C D), (B A C D), (A C B D), (C A B D), (B C A D), (C B A D), (A B D C),...
(define permute
  (lambda (values)
    (lambda ()
      (let ((permutation values))
	permutation))))
