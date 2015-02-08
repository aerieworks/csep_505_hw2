(with* ([x (+ 1 2)]
        [y (* x x)]
        [x (+ y 3)])
  (+ x y))
