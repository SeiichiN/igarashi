(* プログラミング in OCaml 練習問題 14.3 *)

(* 練習問題 14.3 *)

(*
max や map関数の拡張リストに対しての定義を、上の length、alength と同じように、差分（`App の場合のみ）を記述するように定義しなさい。
*)


let l1 = `Cons (1, `Cons (2, `Nil));;
let l2 = `Cons (3, `Cons (4, `Nil));;

let rec map f = function
    `Nil -> `Nil
    | `Cons (a, l) ->
            `Cons((f a), map f l);;
(* val map :
  ('a -> 'b) ->
  ([< `Cons of 'a * 'c | `Nil ] as 'c) -> ([> `Cons of 'b * 'd | `Nil ] as 'd) =
  <fun>  *)

map (fun x -> x * 2) l1;;
(*  - : [> `Cons of int * 'a | `Nil ] as 'a = `Cons (2, `Cons (4, `Nil))  *)

let rec max_list = function
  `Cons (x, `Nil) -> x
  | `Cons (x, `Cons (y, l)) ->
    if x < y then max_list (`Cons(y, l))
    else max_list (`Cons(x, l));;

max_list l2;;

let l6 = `Cons (5, `App (l1, l2));;

let amax_list = function
  Cons (x, l) -> max_list (`Cons (x, l))
  | `App(l1, l2) -> 

max_list l6;;

