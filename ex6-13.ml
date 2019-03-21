(* 練習問題 6.13 *)

(*
step 関数をヒントにして、フィボナッチ数を表す無限列を定義しなさい。

# fib;;
- : intseq = Cons (1, <fun>)

# nthseq 10 fib;;
-: int = 55
 *)

type intseq = Cons of int * (int -> intseq);;

(* step 関数 *)

let rec step n x = Cons (x+n, step (n+1));;
  
let Cons(x1, f1) = step 1 0
let Cons(x2, f2) = f1 x1;;

  (* n番目の要素を取り出す関数 *)
let rec nthseq n (Cons(x,f)) =
  if n = 1 then x
  else nthseq (n-1) (f x);;

let rec fibo x =
  if x < 3 then 1
  else
    fibo (x-1) + fibo (x-2);;
  
let rec fib x =
  if x < 3 then Cons (1, fib)
  else
    Cons (fibo(x-1) + fibo(x-2), fib);;
