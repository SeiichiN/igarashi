(* 練習問題 8.4 *)

(*
参照と繰り返しの構文 (while, for)を使って、フィボナッチ数を求める関数を定義しなさい。
 *)

(* フィボナッチ数の復讐 *)

let rec fib n =
  if n = 1 || n = 2 then 1
  else fib(n-1) + fib(n-2);;

let fib_test1 = fib 5 = 5;;
let fib_test2 = fib 40 = 102334155
  
  (* 上のやり方は効率的でない。fib n の計算のために fib (n-1) と fib (-2) の計算が必要。 *)

let rec fib_pair n =
  if n = 1 then (1, 0)
  else
    let (i, j) = fib_pair (n-1) in
    (i+j, i);;

let fib n =
  let (i, _) = fib_pair n in
  i;;

  (* 上のふたつの関数を合体させる。 *)
let fib n =
  let rec fib_pair n =
    if n = 1 then (1, 0)
    else
      let (i, j) = fib_pair (n-1) in
      (i+j, i)
  in
  let (i, _) = fib_pair n in
  i;;

let fib_test11 = fib 5 = 5;;
let fib_test12 = fib 40 = 102334155;;
  
