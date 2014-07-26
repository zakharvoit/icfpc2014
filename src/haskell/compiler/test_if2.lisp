( defun main ( ) ( a 1 ) )

( defun nmod2 ( n ) (
    - n (* ( / n 2 ) 2 )
) )

( defun a ( n ) (
    if ( = n 1 ) 1
    (
        if ( = ( nmod2 n ) 0 ) (
            if ( = ( nmod2 ( a ( - n 1 ) ) ) 0 ) n
            (
                + n 1
            )
        )
        (
            if ( = ( nmod2 ( a ( - n 1 ) ) ) 1 ) (
                - n 1
            ) n
        )
    )
) )
