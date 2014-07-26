(defun main (world undocumented)
  (cons 0 (clos step))
  )

(defun step (state world)
  (cons 0 (choose-direction
           (!! world 0) ; map
           (!! (!! (!! world 1) 1) 0) ; x
           (!! (!! (!! world 1) 1) 1) ; y
           )
        )
  )

(defun dx () (cons 0 (cons 1 (cons 0 -1))))
(defun dy () (cons -1 (cons 0 (cons 1 0))))

;; Choose first direction which we can go
(defun choose-direction (map x y)
  (choose-direction- 0 map x y)
  )

;; Loop in choose-direction
(defun choose-direction- (i map x y)
  (if (= i 3) 3 ; exit
    (if (ok-pos-p (+ x (!! (dx) i)) (+ y (!! (dy) i)))
        i
      (choose-direction- (+ i 1) map x y)
      )
    )
  )

;; Check if we can go to this position
(defun ok-pos-p (map x y)
  (&&
   ;; Check bounds
   (&& (&& (>= x 0) (>= y 0))
       (&& (< x (length map)) (< y (length (!! map 0)))))

   ;; Check if free
   (free-pos-p (!! (!! map x) y))
   )
  )

(defun free-pos-p (pos)
  (&& (<= 1 pos) (<= pos 4))
  )
