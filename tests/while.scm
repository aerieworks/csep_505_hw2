(with* [(mkrecur (fun (f) (f f)))
        (id (fun (v) v))
        (and (fun (a b) { if a b false }))
        (or (fun (a b) { if a true b }))
        (not (fun (a) { if a false true }))
        (xor (fun (a b) { or (and a (not b)) (and b (not a)) }))

        (- (fun (a b) { + a (* b -1) }))
        (> (fun (a b) { < b a }))
        (<= (fun (a b) { not (> a b) }))
        (>= (fun (a b) { not (< a b) }))

        (pair (fun (x y) { fun (which) { if which x y } }))
        (fst (fun (p) { p true }))
        (snd (fun (p) { p false }))

        (list (fun (v) { pair v (pair false false) }))
        (value (fun (n) { fst n }))
        (next? (fun (n) { fst (snd n) }))
        (next (fun (n) { snd (snd n) }))
        (cons (fun (v n) { pair v (pair true n) }))

        (continue (fun (v) { pair v true } ))
        (break (fun (v) { pair v false } ))
        (continue? (fun (cond cons alt) { if (snd cond) (cons (value cond)) (alt (value cond)) }))

        (while (mkrecur (fun (w)
                             { fun (v f)
                                   { with* [(result (f v))]
                                     (continue? result (fun (v') { w w v' f }) id)
                                     }
                                   })))

        (for (mkrecur (fun (_for)
                           { fun (start until v f)
                                 { if (< start until)
                                      (_for _for (+ start 1) until (f v start) f)
                                      v
                                      }
                                 })))

        (map (mkrecur (fun (_map)
                           { fun (src f)
                                 { with* [(v (f (value src)))]
                                   (if (next? src)
                                       (cons v (_map _map (next src) f))
                                       (list v))
                                   }
                                 })))

        (factorial (mkrecur (fun (fact)
                                 { fun (x)
                                      { if (< x 2)
                                          (+ 0 1)
                                          (* x (fact fact (+ x -1)))
                                          }
                                      })))

        (head (for 1 10 (list 0) (fun (v i) { cons i v })))
        (_ (map head (fun (in) { * in 2 })))
        (result (while 1 (fun (v) { if (< (factorial v) 100) (continue (+ v 1)) (break v) })))
        ]
       result
)
