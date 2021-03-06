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

(* Newton-Raphson 法のプログラム *)

(*
x * x - 2 = 0  となるような関数 f(x) = x*x - 2 について考える。
x(n+1) = x(n) - f(x(n)) / f'(x(n))
       = x(n) - (x(n) * x(n) - 2) / 2 * x(n)

       n=1 のときの x(1) の値を、仮に 5 とすると、

       x(2) = x(1) - (x(1) * x(1) - 2) / 2 * x(1)
            = 5    - ( 5   *  5   - 2) / 2 * 5
            = 5    -       23          / 10
            = 5    -       2.3
            = 2.7

        x(3) = x(2) - (x(2) * x(2) - 2) / 2 * x(2)
             = 2.7  - (2.7  * 2.7  - 2) / 2 * 2.7
             = 2.7  - (7.29        - 2) / 5.4
             = 2.7  -  5.29             / 5.4
             = 2.7  -  0.97962963
             = 1.72037037
*)

(* f(x) の x のときの微分係数を求める
 * dx を 0.1 * 10 のマイナス10乗 として考える
 *)
let deriv f =
    let dx = 0.1e-10 in
    fun x -> (f(x +. dx) -. f(x)) /. dx;;

deriv (fun x -> x *. x) 1.0;;
(* - : float = 2.000000165480742 *)

(* f(x) = 0 となるような関数 f について
 * x が s のときの値を求める。
 * すなわち、x1 が s のときの x2 をもとめる。 *)
let nr f s =
    s -. (f s) /. (deriv f s);;

(* 実行例
 * f(x) = x * x として
 * x1 が 1.0 のときの x2 の値は *)
nr (fun x -> x *. x) 1.0;;
(* - : float = 0.500000041370182058 *)

(* f(x) = 0 となる x をもとめる
 * f -- f(x) = 0 となる f(x)
 * s -- newton式に与える初期値
 *     （f(x)の形によって適当に与えることとする）
 * dx -- どこまで近似値をもとめるか。
 *      0.1e-6 まで求めることとする。 
 * x2 - x1 の値が、dx より小さくなったら解とする。 *)
let rec newton f s =
    let dx = 0.1e-6 in
    let x1 = nr f s in
    let x2 = nr f x1 in
    if abs_float(x2 -. x1) > dx
    then newton f x1
    else x1;;

(* ルート5 を求める *)
newton (fun x -> x *. x -. 5.0) 1.0;;
(* - : float = 2.23606797750364272 *)

(* heihokon -- 平方根を求める
 * n -- 実数で *)
let heihokon n = 
        newton (fun x -> x *. x -. n) 1.0;;


