(* 練習問題 8.1 *)

(*
本文中でもふれたように、ref 型は以下のように定義された、1フィールドの書き換え可能なレコードです。
type 'a ref = { mutable contents : 'a };;
関数 ref、前置演算子 !、中置演算子 := の定義を、レコードに関連した操作で書きなさい。
 *)

type 'a ref = {mutable contents : 'a};;


let ref x = { contents = x };;

let (!) x = x.contents;;
