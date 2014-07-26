(defun main (world undocumented)
  (cons 0 (clos step))
  )

(defun step (state world)
  (twice (choose-direction
          world
          (!! (!! (!! world 1) 1) 0) ; x
          (!! (!! (!! world 1) 1) 1) ; y
          state
          )
         )
  )

(defun twice (x) (cons x x))

(defun dx () (cons 0 (cons 1 (cons 0 (cons -1 0)))))
(defun dy () (cons -1 (cons 0 (cons 1 (cons 0 0)))))

;; Choose first direction which we can go
(defun choose-direction (world x y state)
  ;; check if last direction is ok
  (if (&& (ok-pos-p (!! world 0) (+ x (!! (dx) state)) (+ y (!! (dy) state)))
          (no-ghost-p (!! world 2) (cons (+ x (!! (dx) state))
                                         (+ y (!! (dy) state)))))
      state
    (choose-direction- 0 (!! world 0) (!! world 2) x y)
    )
  )

;; Loop in choose-direction
(defun choose-direction- (i map ghosts x y)
  (if (= i 3) 3 ; exit
    (if (&& (ok-pos-p map (+ x (!! (dx) i)) (+ y (!! (dy) i)))
            (no-ghost-p ghosts (cons (+ x (!! (dx) i)) (+ y (!! (dy) i)))))
        i
      (choose-direction- (+ i 1) map ghosts x y)
      )
    )
  )

;; Check if we can go to this position
(defun ok-pos-p (map x y)
  (&&
   ;; Check bounds
   (&& (&& (>= x 0) (>= y 0))
       (&& (< y (- (length map) 1)) (< x (- (length (!! map 0)) 1))))

   ;; Check if free
   (free-pos-p (!! (!! map y) x))
   )
  )

(defun free-pos-p (pos)
  (> pos 0)
  )

(defun no-ghost-p (ghosts pos)
  (if (atom ghosts) 1
    (&&
     (! (pair-eq-p pos (!! (car ghosts) 1)))
     (no-ghost-near-p (cdr ghosts) pos)
     )
    )
  )

(defun no-ghost-near-p (ghosts pos)
  (no-ghost-near-p- ghosts pos (cons 0 (dx)) (cons 0 (dy)))
  )

(defun no-ghost-near-p- (ghosts pos dx dy)
  (if (atom dx) 1
    (&& (no-ghost-p ghosts (cons (+ (car pos) (car dx)) (+ (cdr pos) (car dy))))
        (no-ghost-near-p- ghosts pos (cdr dx) (cdr dy))
        )
    )
  )
