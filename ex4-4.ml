(* 練習問題 4.4 *)

(*
 * s k k が恒等関数として働く理由を s k k 1 が評価される計算ステップを示すことで説明せよ。
 *)

(* Kコンビネータ *)
let k x y = x;;

let test1 = k 23 45 = 23;;

let const17 = k 17;;

let test2 = const17 4.0 = 17;;

(* Sコンビネータ *)

let s x y z = x z (y z);;

let test3 = s k k 5 = 5;;

let test4 = s k k 1 = 1;;
(* s k k 1 -> k 1 (k 1) -> 1 *)

