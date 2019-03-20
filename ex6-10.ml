(* 練習問題 6.10 *)

以下は、足し算と掛け算からなる数式の構文をあらわした型定義です。

(*
#type arith =
    Const of int |Add of arith * arith | Mul of arith * arith;;

type arith = Const of int | Add of arith * arith | Mul of arith * arith

例えば (3 + 4) X (2 + 5) のような数式は

# let exp = Mul (Add ( Const 3, Const 4), Add ( Const 2, Const 5));;
val exp : arith = Mul (Add ( Const 3, Const 4), Add ( Const 2, Const 5))

とあらわすことができます。 arith から、そのデータが表す式の値を求める関数 eval （型は arith -> int）を定義しなさい。
*)

