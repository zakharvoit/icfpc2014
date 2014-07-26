(defun f (x) (+ x (g x)))
(defun g (x) (* x (f x)))
