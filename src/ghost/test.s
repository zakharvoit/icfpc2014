; Go up if our x-ordinate is even, or down if it is odd.
int 3          ; Get our ghost index in A.
int 5          ; Get our x-ordinate in A.
and a,1        ; Zero all but least significant bit of A.
;; asdfsadf
mov b,a        ; Save A in B because we need to use A to set direction.
mov a,2        ; Move down by default.
jeq LABEL,b,1      ; Don't change anything if x-ordinate is odd.
mov a,0        ; We only get here if x-ordinate was even, so move up.
LABEL: int 0          ; This is line 7, the target of the above jump. Now actually set the direction.
hlt            ; Stop.