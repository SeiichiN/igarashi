(* chap.4 多相性と型推論 *)

let fst_int ((x, y) : int * int) = x;;

(* TEST *)
let test1 = fst_int (1, 3) = 1;;
    
let fst_ifs ((x, y) : (int * float) * string) = x;;

  (* TEST *)
let test2 = fst_ifs ((1, 3.), "abc") = (1, 3.);;

let fst (x, y) = x;;

  (* TEST *)
let test3 = fst (3, 4) = 3;;
let test4 = fst (4, 5.2) = 4;;
let test5 = fst (5, "abc") = 5;;

  (* 恒等関数 *)
let id x = x;;

let test6 = id 3 = 3;;
  


 (* ------------ *)
let average (a, b) =
    (a +. b) /. 2.;;

fun x -> average (x, 1.0);;

let f = fun x -> (x, x);;

fun x -> ((let f = fun y -> (x, y) in f 4), x + 1);;
(* - : int -> (int * int) * int = <fun> *)

(* コンビネータ *)
let k x y = x;;

let const17 = k 17 in const17 4.0;;

let s x y z = x z (y z);;

