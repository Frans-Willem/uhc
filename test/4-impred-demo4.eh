let  g1  ::  ((forall a . a -> a) -> Int) -> Int
     g2  ::  ((Int -> Int) -> Int) -> Int
     id  =   \x ->  x
     f   =   \h  ->  let  x1  =  g1 h
                          x2  =  g2 h
                          h1  =  h id
                     in   h1
in   f
