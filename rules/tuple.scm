(
    (rules
        ((fst (pair @x @y)) @x)
        ((snd (pair @x @y)) @y)
        ((fst (triple @x @y @z)) @x)
        ((snd (triple @x @y @z)) @y)
        ((thrd (triple @x @y @z)) @z)
        ((fst []) 0)
        ((snd []) 0)
    )
    (weights
        (fst 10)
        (snd 20)
    )
)
