(
    (rules
        ((max @x @x) @x)
        ((min @x @x) @x)
        ((max (max @x @y) @z) (max @x (max @y @z)))
        ((min (min @x @y) @z) (min @x (min @y @z)))
    )
    (eqs
        ((max @x @y) (max @y @x))
        ((min @x @y) (min @y @x))
        ((max @x (max @y @z)) (max @z (max @x @y)))
        ((max @z (max @x @y)) (max @x (max @y @z)))
        ((min @x (min @y @z)) (min @z (min @x @y)))
        ((min @z (min @x @y)) (min @x (min @y @z)))
    )
)
