(* 練習問題 6.15 *)

(*
sum型を使って、次の型をもつ関数 f1 〜 f7 を定義しなさい。（組型の * と sum は sum のほうが強く結合します）。
 *)

(*
 1 -- f1: 'a * ('b, 'c) sum -> ('a * 'b, 'a * 'c) sum  
*)

type ('a, 'b) sum = Left of 'a | Right of 'b;;

let f1 (a, x) =
  match x with
    Left b ->  Left (a, b)
  | Right c -> Right (a, c);;
  
let test1 = f1 (3, (Right 5)) = Right (3, 5);;

(*
2 -- f2: ('a * 'b, 'a * 'c) sum -> 'a * ('b, 'c) sum
 *)

let f2 (a, x) =
  match (a, x) with
    Left (a, b) ->  (a,  b)
  | Right (a, c) ->  (a,  c);;

