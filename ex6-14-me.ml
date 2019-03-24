(* 練習問題 6.14 *)

(*
6.5.2で定義した is_prime は、n が素数か判定するのに、n-1 から順に 2 までのすべての数で割り切れるかどうか試していますが、これはあまり効率がよいとはいえません。以下のそれぞれの方法で、3000番目の素数の計算が速くなるかどうか試してみなさい。
1. 割る数として、小さい数のほうから試したいく
2. 割る数の上限を、ルートn の小数点以下の切り捨ての数とする。切り捨ては、OCaml では、floor という関数になる。
3. 割る数として、n より小さい素数だけを試す
4. 割る数として、ルートn の小数点以下切り捨ての素数だけを試す

ヒント：最後のふたつの場合は、prime_seq 関数をこれまでに見つかった素数のリストを引数として取るようにします。

let rec prime_seq primes x = ...

あとは、1, 3, 6, 10, 15, ... という無限列を作った時のテクニックを使います。

# nthseq 20 (prime_seq [] 1);;
- : int = 71

 *)


type intseq = Cons of int * (int -> intseq);;


(* n番目の要素を取り出す関数 *)
let rec nthseq n (Cons(x, f)) =
    if n = 1 then x
    else nthseq (n-1) (f x);;


(* 素数かどうかの判定 *)
(* l -- x 未満の素数のリスト *)
let is_prime3 x =
  let rec is_divible x l =
    match l with
      [] -> false
    | v :: rest ->
     if x mod v = 0 then true   (* 割り切れる = 素数ではない *)
     else is_divible x rest
  in
  not(is_divible x (primes (x-1)));;

(* 与えられた x より大きい最小の素数を返す関数 *)
let rec next_prime x =
    if is_prime3 (x+1) then x+1
    else next_prime (x+1);;


(* x 以下の素数のリストを得る *)
let primes x =
  let rec primes_in n e =
    let next = next_prime n in
    if x <= next then e
    else
      primes_in next (next :: e)
  in
  primes_in 1 [];;
  
  

  

(* 素数の無限列 *)
let rec prime_seq x =
    if is_prime3 (x+1)
    then Cons(x+1, prime_seq)
    else prime_seq (x+1);;

(* let prime = Cons (2, prime_seq [2]);; *) (* 下と同じ *)
let prime = prime_seq 1;;  
