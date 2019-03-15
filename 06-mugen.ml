(* 無限列 *)

type intseq = Cons of int * (int -> intseq);;

let rec f x = Cons (x+1, f);;

let rec step2 x = Cons (x+2, step2);;

let Cons(x1, f1) = step2 0
let Cons(x2, f2) = f1 x1
let Cons(x3, f3) = f2 x2;;

let rec step n x = Cons(x+n, step (n+1));;

let Cons(x1, f1) = step 1 0
let Cons(x2, f2) = f1 x1
let Cons(x3, f3) = f2 x2
let Cons(x4, f4) = f3 x3
let Cons(x5, f5) = f4 x4;;

(* n番目の要素を取り出す関数 *)
let rec nthseq n (Cons(x, f)) =
    if n = 1 then x
    else nthseq (n-1) (f x);;

