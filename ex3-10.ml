(* 練習問題 3.10 *)
(* fib 4 の値呼び出しによる評価ステップを fact 4 の例にならって示しなさい。 *)
(*
フィボナッチ数列
1  2  3  4  5  6  7  8  9 10 11  12
1  1  2  3  5  8 13 21 34 55 89 144
*)

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

  (* TEST *)
let test1 = fib 4 = 3;;
let test2 = fib 7 = 13;;
let test3 = fib 12 = 144;;

  (*
fib 4 = fib_pair 4 = (if 4=1) fib_pair 3 = (L, R) => (L+R, L) = L+R 
        fib_pair 3 = (if 3=1) fib_pair 2 = (L, R) => (L+R, L) = L+R
        fib_pair 2 = (if 2=1) fib_pair 1 = (L, R) => (L+R, L) = L+R
        fib_pair 1 = (if 1=1) (1, 0) => 1
        fib_pair 2 = (1, 1) => 1
        fib_pair 3 = (2, 1) => 2
        fib_pair 4 = (3, 2) => 3
   *)
