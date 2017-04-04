(
    (rules
        ((abs 0) 0)
        ((add (add @x @y) @z) (add @x (add @y @z)))
        ((add 0 @x) @x)
        ((add @x 0) @x)
        ((mult @x 0) 0)
        ((mult 0 @x) 0)
        ((add (mult @x @y) (mult @x @z)) (mult @x (add @y @z)))
        ((sub @x 0) @x)
        ((sub @x @x) 0)
        ((abs (abs @x)) (abs @x))
    )
    (eqs
        ((add @x @y) (add @y @x))
        ((mult @x @y) (mult @y @x))
        ((add @x (add @y @z)) (add @z (add @x @y)))
        ((add @z (add @x @y)) (add @x (add @y @z)))
        ((mult @x (add @y @z)) (mult @z (add @x @y)))
        ((mult @z (mult @x @y)) (add @x (add @y @z)))
    )
    (weights
        (abs 4)
        (add 5)
        (mult 6)
        (sub 7)
        (succ 8)
    )
    (precs
        (abs 4)
        (add 5)
        (mult 6)
        (sub 7)
        (succ 8)
    )
)
