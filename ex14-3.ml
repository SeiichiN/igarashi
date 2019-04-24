(* プログラミング in OCaml 練習問題 14.3 *)

(* 練習問題 14.3 *)

(*
max や map関数の拡張リストに対しての定義を、上の length、alength と同じように、差分（`App の場合のみ）を記述するように定義しなさい。
*)


let l1 = `Cons (1, `Cons (2, `Nil));;
let l2 = `Cons (3, `Cons (4, `Nil));;
let l6 = `Cons (5, `App (l1, l2));;
let l7 = `Cons (10, `Cons (17, `App (l1, l2)));;

(* ふつうの max_list -- list の中の最大値を求める *)
let rec max_list = function
  `Cons (x, `Nil) -> x
  | `Cons (x, `Cons (y, l)) ->
    if x < y then max_list (`Cons(y, l))
    else max_list (`Cons(x, l));;

max_list l2;;  (* - : int = 4 *)

(* `App を含まないリストのための make_max 関数 *)
let make_max f = function
  `Cons (x, `Nil) -> x
  | `Cons (x, `Cons (y, l)) ->
    if x < y then f (`Cons(y, l))
    else f (`Cons(x, l));;
(* val make_max :
  ([> `Cons of 'a * 'b ] -> 'a) ->
  [< `Cons of 'a * [< `Cons of 'a * 'b | `Nil ] ] -> 'a = <fun>
*)

(* `App を含まないリストの最大値を求める *)
let rec max_list l = make_max max_list l;;
(*val max_list : [ `Cons of 'a * ([< `Cons of 'a * 'b | `Nil ] as 'b) ] -> 'a =
  <fun> *)

max_list l2;;  (* - : int = 4 *)

(* make_amax 関数のための max 関数 -- ふつうのリストの中の最大値を求める  *)
let max l = 
    let rec max_in l e =
        match l with
        [] -> e
  | v :: rest ->
          if v > e then max_in rest v
          else max_in rest e
    in
    max_in l 0;;
(* val max : int list -> int = <fun> *)

(* `App を含む拡張リストのための make_amax 関数 *)
let make_amax f = function
    (`Cons (_, `Nil) | `Cons (_, `Cons(_, _))) as l -> make_max f l
  | `Cons (x, `App (l1, l2)) ->
          let a1 = f l1 and a2 = f l2 in
          max [x; a1; a2];;
(* val make_amax :
  (([> `Cons of int * 'b ] as 'a) -> int) ->
  [< `Cons of int * [< `App of 'a * 'a | `Cons of int * 'b | `Nil ] ] -> int =
  <fun>  *)

(* `App を含む拡張リストの最大値を求める関数 *)
let rec amax_list l = make_amax amax_list l;;
(* val amax_list :
  ([ `Cons of int * ([< `App of 'a * 'a | `Cons of int * 'b | `Nil ] as 'b) ]
   as 'a) ->
  int = <fun>  *)

amax_list l6;;  (* - : int = 5  *)
amax_list l7;;  (* - : int = 17 *)

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
