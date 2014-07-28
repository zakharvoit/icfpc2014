(defun main (world undocumented)
  (cons 0 (clos step))
  )

;; Getters for world
(defun map-g (world) (!! world 0))
(defun lambdaman-g (world) (!! world 1))
(defun ghosts-g (world) (!! world 2))
(defun fruit-g (world) (!! world 3))
(defun cell-g (world pos) (!! (!! (map-g world) (y pos)) (x pos)))
(defun pos-g (world) (!! (lambdaman-g world) 1))

(defun step (state world)
  (twice (choose-direction
          world
          state
          )
         )
  )

;; Points useful functions
(defun dx-l () (cons 0 (cons 1 (cons 0 (cons -1 0)))))
(defun dy-l () (cons -1 (cons 0 (cons 1 (cons 0 0)))))

(defun dx (i) (!! (dx-l) i))
(defun dy (i) (!! (dy-l) i))
(defun nx (x i) (+ x (dx i)))
(defun ny (y i) (+ y (dy i)))
(defun npos (pos i) (cons (nx (car pos) i) (ny (cdr pos) i)))
(defun x (pos) (car pos))
(defun y (pos) (cdr pos))

;; List length (length of tuple - 1)
(defun llength (l) (- (length l) 1))

;; Choose first direction which we can go
(defun choose-direction (world state)
  ;; check if last direction is ok
  (if (&& (ok-pos-p world (npos (pos-g world) state))
          (no-ghost-p (ghosts-g world) (npos (pos-g world) state)))
      state
    (choose-direction- 0 world)
    )
  )

;; Loop in choose-direction
(defun choose-direction- (i world)
  (if (= i 3) 3 ; exit
    (if (&& (ok-pos-p world (npos (pos-g world) i))
            (no-ghost-p (ghosts-g world) (npos (pos-g world) i)))
        i
      (choose-direction- (+ i 1) world)
      )
    )
  )

;; Check if we can go to this position
(defun ok-pos-p (world pos)
  (&&
   ;; Check bounds
   (&& (&& (>= (x pos) 0) (>= (y pos) 0))
       (&& (< (y pos) (llength (map-g world)))
           (< (x pos) (llength (!! (map-g world) 0)))))

   ;; Check if free
   (free-pos-p (cell-g world pos))
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
  (no-ghost-near-p- ghosts pos (cons 0 (dx-l)) (cons 0 (dy-l)))
  )

(defun no-ghost-near-p- (ghosts pos dx dy)
  (if (atom dx) 1
    (&& (no-ghost-p ghosts (cons (+ (car pos) (car dx)) (+ (cdr pos) (car dy))))
        (no-ghost-near-p- ghosts pos (cdr dx) (cdr dy))
        )
    )
  )
