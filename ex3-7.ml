(* 練習問題 3.7 *)
(* x は実数、n は ゼロ以上の整数として、x の n乗 を計算する pow(x,n)を
以下の2通りで定義しなさい。*)

(* 1) 計算中に pow の（再帰）呼び出しをn回伴う定義 *)

(* 2) 計算中の pow の（再帰）呼び出しが約 log2n 回ですむ定義 *)
(* ヒント：x の 2n乗 = (xの2乗)のn乗 ですね。では、xの(2n+1)乗は？ *)

let rec pow (x, n) =
  let rec interpow (n, res) =
    if n = 1 then res
    else
      interpow (n-1, res *. x)
  in
  interpow(n, x);;

  (* TEST *)
let test1 = pow(2., 3) = 8.;;
let test2 = pow(3., 3) = 27.;;

let rec pow2 (x, n) =
  if n = 1 then x
  else
    pow2(x, n-1) *. x;;

let test3 = pow2(2., 3) = 8.;;
let test4 = pow2(3., 3) = 27.;;
  
let rec pow3 (x, n) =
  if n = 1 then x
  else
    if n mod 2 = 0
    then pow3 (x *. x, n / 2)
    else pow3 (x *. x, (n-1) / 2) *. x;;

let test5 = pow3(2., 3) = 8.;;
let test6 = pow3(3., 3) = 27.;;

