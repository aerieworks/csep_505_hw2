(with* [(mkrecur (fun (f) (f f)))

        (incBy (fun (b amount) { set-box! b (+ (unbox b) amount) }))
        (getAndIncBy (fun (b amount) { with* [(old (unbox b)) (_ (incBy b amount))] old }))
        (inc (fun (b) { incBy b 1 }))
        (getAndInc (fun (b) { getAndIncBy b 1 }))
        (dec (fun (b) { incBy b -1 }))
        (getAndDec (fun (b) { getAndIncBy b -1 }))

        (pair (fun (x y) { fun (which) { if which x y } }))
        (fst (fun (p) { p true }))
        (snd (fun (p) { p false }))

        (list (fun (v) { pair v (pair false false) }))
        (value (fun (n) { fst n }))
        (next? (fun (n) { fst (snd n) }))
        (next (fun (n) { snd (snd n) }))
        (cons (fun (v n) { pair v (pair true n) }))

        (while (with* [(_while (mkrecur (fun (w)
                                        { fun (v f)
                                              { with* [(result (f v))]
                                                (if (fst result)
                                                    (w w (snd result) f)
                                                    result)
                                                }
                                              })))]
               (fun (v f) { snd (_while v f) })))

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
        (result (map head (fun (in) { * in 2 })))
        ]
       (value result)
)
