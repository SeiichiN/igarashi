(* 無限列 *)

(* 
 * intseq -- int と intを引数にしてintseqにする関数の組
 *)
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

(* 素数かどうかを調べる関数 *)
let is_prime x =
    let rec is_divisible_from_2_to n =
        (n > 1) && ((x mod n = 0) || is_divisible_from_2_to (n-1))
    in
    not (is_divisible_from_2_to (x-1));;

(* 与えられた x より大きい最小の素数を返す関数 *)
let rec next_prime x =
    if is_prime (x+1) then x+1
    else next_prime (x+1);;

(* 素数の無限列 *)
let rec prime_seq x =
    if is_prime (x+1)
    then Cons(x+1, prime_seq)
    else prime_seq (x+1);;

let test_nth_prime = nthseq 20 (prime_seq 1) = 71;;

