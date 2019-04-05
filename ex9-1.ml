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
