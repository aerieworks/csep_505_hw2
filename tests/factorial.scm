((with* [(mkfact (fun (fact) (fun (x) (if (< x 2) 1 (* x (fact fact (+ x -1)))))))]
        (mkfact mkfact)
        )
 10)
