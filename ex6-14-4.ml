(* 練習問題 6.14 *)

(*
6.5.2で定義した is_prime は、n が素数か判定するのに、n-1 から順に 2 までのすべての数で割り切れるかどうか試していますが、これはあまり効率がよいとはいえません。以下のそれぞれの方法で、3000番目の素数の計算が速くなるかどうか試してみなさい。
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



(* 素数の無限列 *)
let rec prime_seq primes x =
  let x' = x + 1 in
  let rec is_prime n = function
      [] -> true
    | p :: rest ->
       if p = n then true
       else
         p < n && n mod p <> 0 && is_prime n rest
  in
  if is_prime x' primes
  then Cons (x', prime_seq (x' :: primes))
  else prime_seq primes x';;
  
(* let prime = Cons (2, prime_seq [2]);; *) (* 下と同じ *)
let prime = prime_seq [] 1;;  

let rec is_prime n = function
    [] -> true
  | p :: rest ->
     if p*p > n then true
     else
       p*p <= n && n mod p <> 0 && is_prime n rest;;

(* 実行例 *)
let test1 = nthseq 10 prime = 29;;
let test2 = nthseq 3000 prime = 27449;; (* 4秒位かかった *)

