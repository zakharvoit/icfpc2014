;; Get ith element of list
(defun !! (list idx)
  (if (= idx 0)
      (if (atom list) list (car list))
    (!! (cdr list) (- idx 1))
    )
  )

(defun length (list)
  (if (atom list) 1
    (length (cdr list))
    )
  )

(defun && (a b)
  (if (= a 0) 0 (if (= b 0) 0 1))
  )

(defun || (a b)
  (if (= a 1) 1 (if (= b 1) 1 0))
  )
