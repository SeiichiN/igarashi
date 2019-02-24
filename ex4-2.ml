(* 練習問題 4.2 *)
(* フィボナッチ数列を計算する関数 fib を定義せよ。 *)

let rec repeat f n x =
    if n > 0
    then repeat f (n-1) (f x)
    else x;;

let twice x = x * 2;;
let test1 = repeat twice 5 2 = 64;;

let fib n =
    let (fibn, _) =
        repeat (fun (x, y) -> (x+y, x)) n (0, 1)
    in fibn;;

(*
let rec fib n =
    if n = 1 then 1
    else
        if n = 2 then 1
        else
            fib (n-2) + fib (n-1);;
*)

let test2 = fib 5 = 5;;
let test3 = fib 10 = 55;;

