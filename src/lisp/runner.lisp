(defun runner ()
  (step 0 (world))
  )

(defun world ()
  (cons (field) (cons (lambdaman) (cons (ghosts) 0)))
  )

(defun lambdaman ()
  (cons 3 (cons (cons 2 1) 0))
  )

;; [ [ 0, 0, 0 ]
;; , [ 0, 2, 2 ]
;; ]
(defun field ()
  (cons (cons 0 (cons 0 (cons 0 0)))
        (cons (cons 0 (cons 2 (cons 2 0)))
              0)
        )
  )

(defun ghosts ()
  (cons (cons 1 (cons (cons 2 1) 0)) 0)
  )
