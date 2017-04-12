(
    (rules
        ((cond @b @x @x) @x)
        ((cond true @x @y) @x)
        ((cond false @x @y) @y)
        ((cond_int @b @x @x) @x)
        ((cond_int true @x @y) @x)
        ((cond_int false @x @y) @y)
    )
    (weights
        (cond_int 1)
        (true 1)
        (false 1)
    )
    (precs
        (cond_int 20)
        (true 0)
        (false 1)
        (a 2)
        (b 3)
        (c 4)
        (d 5)
        (e 6)
    )

)
