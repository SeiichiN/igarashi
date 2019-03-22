(* 練習問題 6.13 *)

(*
step 関数をヒントにして、フィボナッチ数を表す無限列を定義しなさい。

# fib;;
- : intseq = Cons (1, <fun>)

# nthseq 10 fib;;
-: int = 55
 *)

type intseq = Cons of int * (int -> intseq);;

let rec fibo x =
  if x < 3 then 1
  else
    fibo (x-1) + fibo (x-2);;
  
let rec fib x =
    if x < 3 then Cons (1, fib)
    else
      Cons (fibo(x-1) + fibo(x-2), fib);;


let nthseq n f =
  let nthseq_in n (Cons (x, f)) =
    if n < 3 then 1
    else x
  in
  nthseq_in n (f n);;

(* 正解 http://gifnksm.hatenablog.jp/entry/20080316/1205700453 *)
let fib =
  let rec f x y = Cons (x, f (x+y)) in
  Cons (1, f 1);;

let rec nthseq n (Cons (x, f)) =
  if n = 1 then x
  else nthseq (n-1) (f x);;

