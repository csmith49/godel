(
    (rules
        ((join (split @x @y) @y) @x)
        ((to_upper (to_upper @x)) (to_upper @x))
        ((to_lower (to_lower @x)) (to_lower @x))
        ((trim (trim @x)) (trim @x))
        ((add (length @x) (length @y)) (length (concat @x @y)))
        ((concat (to_upper @x) (to_upper @y)) (to_upper (concat @x @y)))
        ((concat (to_lower @x) (to_lower @y)) (to_lower (concat @x @y)))
        ((reverse (reverse @x)) @x)
        ((to_int (of_int @x)) @x)
        ((of_int (to_int @x)) @x)
    )
)
