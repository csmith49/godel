(
    (rules
        ((add (sum @x) (sum @y)) (sum (cat @x @y)))
        ((add (length @x) (length @y)) (length (cat @x @y)))
        ((cat (mkList @x) @y) (cons @x @y))
    )
)
