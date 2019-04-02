(* 練習問題 8.2 *)

(*
 与えられた参照の指す先の整数を 1 増やす関数 incr を定義しなさい。

 # let x = ref 3;;
 val x : int ref = {contents = 3}

 # incr x;;
 - : unit = ()

 # !x;;
 - : int 4

 *)

type 'a ref = { mutable contents : 'a };;

let incr x = x := !x + 1;;

(* 実行例 *)
let p = ref 5;;
p;;
incr p;;
p;;

