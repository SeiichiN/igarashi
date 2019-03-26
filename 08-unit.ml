(* unit *)

(* unit 型を使った無限列表現 *)

type 'a seq = Cons of 'a * (unit -> 'a seq);;

let rec from n = Cons (n, fun () -> from (n+1));;

let Cons(x1, f1) = from 1
let Cons(x2, f2) = f1 ()
let Cons(x3, f3) = f2 ();;

let rec mapseq f (Cons (x, tail)) =
  Cons (f x, fun () -> mapseq f (tail ()));;

let reciprocals = mapseq (fun x -> 1.0 /. float_of_int x) (from 2);;

  (* take -- 無限列のはじめの n 要素をリストにする関数 *)
(*
#  take 5 reciprocals;;
  - : float list = [0.5; 0.333333333333315; 0.25; 0.2; 0.166666666666657]
                     
 *)
  
