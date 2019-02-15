(* 再帰 *)

let rec fact n = (* n の階乗 *)
    if n = 1 then 1 else fact (n-1) * n;;

(* n番目のフィボナッチ数 *)
let rec  fib n =
    if n = 1 || n = 2
    then 1
    else fib(n - 1) + fib(n - 2);;

(*  効率が悪いので改善する *)
let rec fib_pair n =
    if n = 1
    then (1, 0)
    else
        let (i, j) = fib_pair (n - 1) in
        (i + j, i);;

let fib n = 
    let (i, _) = fib_pair n in 
    i;;

let fib n = 
    let rec fib_pair n =
        if n = 1
    then (1, 0)
    else
        let (i, j) = fib_pair (n - 1) in
        (i + j, i) 
        in
    let (i, _) = fib_pair n in 
    i;;

(* ユークリッドの互除法 *)
(*
ふたつの自然数 m と n の最大公約数を求める。ただし、m <= n
m = n の場合は、m が最大公約数。
m <> n の場合は、n - m と m の最大公約数。
*)
let rec maxyaku (m, n) =
    if m = n
    then m
    else
        maxyaku ((n - m), m);;

