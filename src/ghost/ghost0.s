;;; This ghost tries to go to direction in which lambdaman is now
int 1        ;; fetch lambdaman position
mov [255], A ;; lambdaman x
mov [254], B ;; lambdaman y

int 3
int 4        ;; fetch our position
mov [253], A ;; ghost x
mov [252], B ;; ghost y

;;; [251] will be answer

;;; Choose dx
chooseDx: jlt right, [253], [255]
          jeq chooseDy, [253], [255]
          jeq left, 0, 0

left:   mov [251], 0
        jeq end, 0, 0

right:  mov [251], 1
        jeq end, 0, 0

;;; Choose dy
chooseDy: jlt down, [252], [254]
          jeq up, 0, 0

down:   mov [251], 2
        jeq end, 0, 0

up:     mov [251], 3
        jeq end, 0, 0

;;; (dx, dy) -> direction
end:    mov A, [251]
        int 0
        hlt
