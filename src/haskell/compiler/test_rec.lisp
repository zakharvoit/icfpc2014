;;; http://oeis.org/A004001
(defun main () (a 5))
(defun a (n) (if (<= n 2) 1 (+ (a (a (- n 1))) (a (- n (a (- n 1)))))))
