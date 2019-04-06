(* プログラミング in OCaml 練習問題 9.1 *)

(* 練習問題 9.1 *)

(*
（任意精度の）有理数計算を行う Num モジュールを使って、3.6.5 節で取り上げた Newton-Raphson 法のプログラムを書き直しなさい。いくつか使うであろう関数を下に挙げておきます。

 # open Num;;

 # Int 2;; (* 整数から num への変換のためのコンストラクタ  *)
 - : Num.num = int 2

 # string_of_num;;  (* 文字列への変換 *)
 - : Num.num -> string = <fun>

 # ( **/ );;  (* 指数関数 *)
 - : Num.num -> Num.num -> Num.num = <fun>

 # (=/);;   (* 等しさの比較 *)
 - : Num.num -> Num.num -> bool = <fun>

 # (</);;  (* 大小比較 *)
 - : Num.num -> Num.num -> bool = <fun>

 # abs_num;;   (* 絶対値 *)
 - : Num.num -> Num.num = <fun>

 # Num.approx_num_fix;; (* 有理数から固定小数点表現への変換（第1引数は桁数） *)
 - : int -> Num.num -> string = <fun>

 # Num.approx_num_exp;;  (* 有理数から浮動小数点表現への変換（第1引数は桁数） *)
 - : int -> Num.num -> string = <fun>

 *)

(* Num ライブラリを使う *)
#load "nums.cma";;
open Num;;

let keta = 10;;  (* 小数点の桁数 *)

(* Newton-Raphson 法のプログラム *)

(* f(x) の x のときの微分係数を求める
 * dx を 0.1 * 10 のマイナス10乗 として考える
 *)
let deriv f =
    let dx = Int 1 // Int 10000000000 in
    fun x -> (f(x +/ dx) -/ f(x)) // dx;;

approx_num_fix keta (deriv (fun x -> x **/ Int 2) (Int 1));;
(* - : string = "+2.00000" *)

(* f(x) = 0 となるような関数 f について
 * x が s のときの値を求める。
 * すなわち、x1 が s のときの x2 をもとめる。 *)
let nr f s =
    s -/ (f s) // (deriv f s);;

(* 実行例
 * f(x) = x * x として
 * x1 が 1.0 のときの x2 の値は *)
approx_num_fix keta (nr (fun x -> x **/ Int 2) (Int 1));;
(* - : string = "+0.50000" *)

(* f(x) = 0 となる x をもとめる
 * f -- f(x) = 0 となる f(x)
 * s -- newton式に与える初期値
 *     （f(x)の形によって適当に与えることとする）
 * dx -- どこまで近似値をもとめるか。
 *      0.1e-6 まで求めることとする。 
 * x2 - x1 の値が、dx より小さくなったら解とする。 *)
let rec newton f s =
    let dx = Int 1 // Int 10000000000 in
    let x1 = nr f s in
    let x2 = nr f x1 in
    if abs_num(x2 -/ x1) >/ dx
    then newton f x1
    else x1;;

(* ルート5 を求める *)
approx_num_fix keta (newton (fun x -> x **/ Int 2  -/ Int 5) (Int 1));;
(* - : string = "+2.2360679775" *)

(* heihokon -- 平方根を求める
 * n -- 整数 *)
let heihokon n = 
        approx_num_fix keta (newton (fun x -> x **/ Int 2 -/ (Int n)) (Int 1));;

heihokon 2;;
(* - : string = "+1.4142135624" *)
