(* 練習問題 6.14.1 *)

(*
6.5.2で定義した is_prime は、n が素数か判定するのに、n-1 から順に 2 までのすべての数で割り切れるかどうか試していますが、これはあまり効率がよいとはいえません。以下のそれぞれの方法で、3000番目の素数の計算が速くなるかどうか試してみなさい。
1. 割る数として、小さい数のほうから試したいく

 *)


type intseq = Cons of int * (int -> intseq);;


(* n番目の要素を取り出す関数 *)
let rec nthseq n (Cons(x, f)) =
    if n = 1 then x
    else nthseq (n-1) (f x);;


(* 素数かどうかを調べる。小さい数から順に *)
let is_prime1 x =
    let rec is_divisible_from_2_to n =
        if n = x then false
        else
            (n > 1) && ((x mod n = 0) || is_divisible_from_2_to (n+1))
    in
    not (is_divisible_from_2_to 2);;


(* 素数の無限列 *)
let rec prime_seq x =
    if is_prime1 (x+1)
    then Cons(x+1, prime_seq)
    else prime_seq (x+1);;


(* 素数の無限列をつくる *)
let prime = prime_seq 1;;
  
let test1 = nthseq 10 prime = 29;;

let test2 = nthseq 3000 prime = 27449;; (* 3秒くらい *)
  
