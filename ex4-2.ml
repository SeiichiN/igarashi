(* 練習問題 4.2 *)

let rec repeat f n x =
    if n > 0
    then repeat f (n-1) (f x)
    else x;;

let twice x = x * 2;;
let test1 = repeat twice 5 2 = 64;;

let fib n =
    let (fibn, _) = ...
    in fibn;;

