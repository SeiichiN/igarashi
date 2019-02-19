(* 練習問題 3.11 *)

(* ユークリッドの互除法 *)
(*
ふたつの自然数 m と n の最大公約数を求める。ただし、m <= n
m = n の場合は、m が最大公約数。
m <> n の場合は、n - m と m の最大公約数。
*)
let rec gcd (m, n) =
    if m = n
    then m
    else
      if (m > n)
      then
        gcd(m - n, n)
      else
        gcd(n - m, m);;

(*  TEST  *)
let test1 = gcd (100, 25) = 25;;
let test2 = gcd (20, 20) = 20;;
let test3 = gcd (45, 12) = 3;;

(* 組み合わせ *)
(*
m個のなかからn個選び出す組み合わせの数 mCn は、
mCn = mPn / n!
すなわち、
mPn = m! / (m - n)!  --> これを n! で割る。
たとえば、5C2 なら、
5P2 = 5! / (5-2)! = 5*4*3*2*1 / 3*2*1 = 20
5C2 = 5P2 / n! なので、20 / 2*1 = 10
答え、10通り
*)
(*
以下のように、再帰的に定義できる。
nC0 = nCn = 1
nCm = (n-1)Cm + (n-1)C(m-1)
ただし、0 <= m < n
*)
(* comb : (int, int) => int *)
let rec comb (n, m) =
  if m = 0 || n = m
  then 1
  else
    comb ((n - 1), m) + comb ((n - 1), (m - 1));;

(* TEST *)
let test11 = comb(5, 2) = 10;;
let test12 = comb(10, 2) = 45;;
let test13 = comb(10, 4) = 210;;

  
