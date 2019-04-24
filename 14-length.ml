(* 第14章 多相ヴァリアント *)


(****************** 14.3 再帰的関数と多相ヴァリアント *****************)

(* ------------- 14.3.1 多相ヴァリアントによるリストの構成 ----------- *)

let l1 = `Nil
and l2 = `Cons(1, `Nil)
and l3 = `Cons(2, `Cons(1, `Nil));;
(* val l1 : [> `Nil ] = `Nil
val l2 : [> `Cons of int * [> `Nil ] ] = `Cons (1, `Nil)
val l3 : [> `Cons of int * [> `Cons of int * [> `Nil ] ] ] =
  `Cons (2, `Cons (1, `Nil)) *)

fun x -> if x then l1 else l2;;
(* - : bool -> [> `Cons of int * [> `Nil ] | `Nil ] = <fun> *)

let l4 = `Cons(true, `Cons(1, `Nil))
and l5 = `Cons(`Cons(1, `Nil));;
(* val l4 : [> `Cons of bool * [> `Cons of int * [> `Nil ] ] ] =
  `Cons (true, `Cons (1, `Nil))
val l5 : [> `Cons of [> `Cons of int * [> `Nil ] ] ] = `Cons (`Cons (1, `Nil))  *)

let rec length = function
    `Nil -> 0
    | `Cons (a, l) -> 1 + length l;;
(* val length : ([< `Cons of 'b * 'a | `Nil ] as 'a) -> int = <fun> *)

List.map length [l1; l2; l3];;
(* - : int list = [0; 1; 2] *)

(* length l4;; *)
(* Error: This expression has type
         [> `Cons of bool * [> `Cons of int * [> `Nil ] ] ]
       but an expression was expected of type
         [< `Cons of bool * 'a | `Nil ] as 'a
       Types for tag `Cons are incompatible *)

(* length l5;;  *)
(* Error: This expression has type
         [> `Cons of bool * [> `Cons of int * [> `Nil ] ] ]
       but an expression was expected of type
         [< `Cons of bool * 'a | `Nil ] as 'a
       Types for tag `Cons are incompatible *)


(* ------ 14.3.2 再帰的関数の拡張 ------ *)

(* 拡張リスト構造 *)
(* `App -- ふたつの `Cons リストを結合する *)
let l6 = `Cons (1, `App (l2, l3));;
(* val l6 :
 *   [> `Cons of
 *      int *
 *      [> `App of
 *         [> `Cons of int * [> `Nil ] ] *
 *         [> `Cons of int * [> `Cons of int * [> `Nil ] ] ] ] ] =
 *  `Cons (1, `App (`Cons (1, `Nil), `Cons (2, `Cons (1, `Nil))))  *)

(* 拡張リスト構造の長さを求める *)
let rec alength = function
  `Nil -> 0
  | `Cons (a, l) -> 1 + alength l
  | `App (l1, l2) -> alength l1 + alength l2;;
(* val alength : ([< `App of 'a * 'a | `Cons of 'b * 'a | `Nil ] as 'a) -> int =
 *   <fun>  *)

let rec alength_wrong = function
  (`Nil | `Cons (_, _)) as l -> length l
  | `App (l1, l2) -> alength_wrong l1 + alength_wrong l2;;
(* val alength_wrong :
 *   ([< `App of 'a * 'a
 *     | `Cons of 'b * ([ `Cons of 'b * 'c | `Nil ] as 'c)
 *     | `Nil ]
 *   as 'a) ->
 *   int = <fun>  *)

(*  alength_wrong l6;;  *)

let make_length f = function
  `Nil -> 0
  | `Cons (a, l) -> 1 + f l;;
(*  val make_length : ('a -> int) -> [< `Cons of 'b * 'a | `Nil ] -> int = <fun> *)

let rec length l = make_length length l;;
(* val length : ([< `Cons of 'b * 'a | `Nil ] as 'a) -> int = <fun>  *)

length l3;;  (* - : int = 2  *)

let make_alength f = function
  (`Nil | `Cons (_, _)) as l -> make_length f l
  | `App (l1, l2) -> f l1 + f l2;;
(* val make_alength :
 *   ('a -> int) -> [< `App of 'a * 'a | `Cons of 'b * 'a | `Nil ] -> int = <fun> *)

let rec alength l = make_alength alength l;;
(* val alength : ([< `App of 'a * 'a | `Cons of 'b * 'a | `Nil ] as 'a) -> int =
 *   <fun> *)

alength l6;;  (* - : int = 4 *)

type ('a, 'b) mylist = [`Nil | `Cons of 'a * 'b];;

let make_alength f = function
    #mylist as l -> make_length f l
  | `App (l1, l2) -> f l1 + l2;;

let rec alength l = make_alength l;;

