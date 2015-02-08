(with* ([b (box 0)]
        [inc! (fun (n) (set-box! b (+ n (unbox b))))]
        [_ (inc! 5)]
        [_ (inc! 3)])
  (unbox b))
