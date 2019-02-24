(* 練習問題 4.6 *)
(*
 * 恒等関数を s k k で表現したように、関数 fun x y -> y を、
 * コンビネータ s と k を関数適用のみで、（fun や let による
 * 関数定義を使わずに）組み合わせた形で表現せよ。
 *)

      
(* Kコンビネータ *)
let k x y = x;;

let test1 = k 23 45 = 23;;

(* Sコンビネータ *)
let s x y z = x z (y z);;

let test3 = s k k 5 = 5;;

let test4 = (fun x y ->  y) 2 4 = 4;;

let f x y = k (s k k) x y;;

let test5 = f 3 4 = 4;;

let test6 = k (s k k) 3 4 = 4;;

