;; Get ith element of list
(defun !! (list idx)
  (if (= idx 0)
      (if (atom list) list (car list))
    (!! (cdr list) (- idx 1))
    )
  )

(defun length (list)
  (if (atom list) 1
    (+ (length (cdr list)) 1)
    )
  )

(defun pair-eq-p (a b)
  (&& (= (car a) (car b)) (= (cdr a) (cdr b)))
  )

;; Bitwise not
(defun ! (a)
  (= a 0)
  )

(defun twice (x) (cons x x))

(defun trace-this (x) (trace x x))
