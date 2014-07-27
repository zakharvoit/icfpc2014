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
