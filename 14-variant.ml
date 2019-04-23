(* 第14章 多相ヴァリアント *)

(********** 14.1 多相ヴァリアントの基本 *****************)

type modverb =  (* 英語の助動詞 *)
    Can | Will | May | Must;;
  (* type modverb = Can | Will | May | Must *)

type container =  (* いれもの *)
    Can | Bottle;;
  (* type container = Can | Bottle *)

type month =
    January | February | March | April | May | June | July | August | September
    | October | November | December;;
  (*
 type month =                                                                             
    January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
 *)

  (* Can や May は、あとから定義されたコンストラクタとしてしか使えない。 *)
  (* ヴァリアントはコンストラクタと型の関係が固定されている。この問題を解決するのが
多相ヴァリアント。
すなわち、ひとつのコンストラクタを複数の型のために使われることを許す仕組み *)
  
  (* -------- 14.1.1 宣言せずに使うコンストラクタ ---------- *)

  (* 多相ヴァリアントは、型宣言・定義をせずにコンストラクタを使うことができる。 *)

  (* ふつうのヴァリアントと区別するために、名前の前に ` （バッククオート）をつける *)
`Can;;  (* - : [> `Can ] = `Can *)
`May;;  (* - : [> `May ] = `May  *)
`Can 2;; (* - : [> `Can of int ] = `Can 2 *)
`April ("fool", false);;
(* - : [> `April of string * bool ] = `April ("fool", false) *)


fun x -> if x then `Can else `May;;
(* - : bool -> [> `Can | `May ] = <fun> *)

[`Can; `May];;
(* - : [> `Can | `May ] list = [`Can; `May] *)

[`Can 2; `Bottle; `Can 3];;
(* - : [> `Bottle | `Can of int ] list = [`Can 2; `Bottle; `Can 3] *)

let c = `Can and m = `May and b = `Bottle;;
(* val c : [> `Can ] = `Can
 * val m : [> `May ] = `May
 * val b : [> `Bottle ] = `Bottle  *)

[c; m];;
(* - : [> `Can | `May ] list = [`Can; `May] *)

[c; b];;
(* - : [> `Bottle | `Can ] list = [`Can; `Bottle] *)

(* 同じ名前のコンストラクタに異なる型の値を与えたものを混ぜて使うことはできない *)

(* fun x -> if x then `Can 2 else `Can true;; *)
(* Error: This expression has type bool but an expression was expected of type int *)

(* [`Can 2; `Can true];; *)
(* Error: This expression has type bool but an expression was expected of type int *)

(* 組の場合は、ＯＫ *)
(`Can 2, `Can true);;
(* - : [> `Can of int ] * [> `Can of bool ] = (`Can 2, `Can true) *)

(***********************************************************************
 * 同じ型を要求する場所に、異なる型の値を引数とする同じ名前のコンストラクタが来てはいけない。
 * 
 ***********************************************************************)

(* 多相ヴァリアントの使用例 *)
let kani = function
    `Right -> "walking to the right"
    | `Left -> "walking to the left";;
(* val kani : [< `Left | `Right ] -> string = <fun> *)

kani `Right;;  (* - : string = "walking to the right" *)

let hito = function
    `Right -> "walking to the right"
    | `Left -> "walking to the left"
    | `Forward -> "walking forward"
    | `Backward -> "walking backward";;
(* val hito : [< `Backward | `Forward | `Left | `Right ] -> string = <fun> *)

hito `Backward;;
(* - : string = "walking backward" *)

(************************************************************************
 * [> `Can] という型 -- 動く範囲が制限された型変数
 * すなわち、
 *   []の中に `Can （しかも of ... がない）を必ず含むような任意の型
 ************************************************************************)

let c : [>`Can] = `Can and m = `May in [m; c];; (* <== OK *)

(* let c : [`Can] = `Can and m = `May in [m; c];; *) (* <== Error *)

(* > -- 動く範囲となる型は、> の右側のコンストラクタをすべて含む（以上） *)

(*
 * [< `Can | `May] -- []の中に `Can と `May しか含まれないような型
 *)

let mirror = function
    `Right -> `Left
    | `Left -> `Right;;
(* val mirror : [< `Left | `Right ] -> [> `Left | `Right ] = <fun> *)

let mirror = function
    `Right -> `Left
    | `Left -> `Right
    | x -> x;;
(* val mirror : ([> `Left | `Right ] as 'a) -> 'a = <fun> *)

(*
[> ...] や [< ...] は、型変数である証拠
 *)

let add_A x = `A :: x;;
(* val add_A : ([> `A ] as 'a) list -> 'a list = <fun> *)

(*   add_A [`A 1];;   <== error *)

let c = ref [`Can];;
(* val c : _[> `Can ] list ref = {contents = [`Can]} *)

c := (`May : [`Can | `May]) :: !c;;
(*  - : unit = ()  *)

!c;;
(* - : [ `Can | `May ] list = [`May; `Can]   *)

(*    c := `Will :: !c;;  *)
(* Error: This expression has type [> `Will ]
       but an expression was expected of type [ `Can | `May ]
       The second variant type does not allow tag(s) `Will  *)

let c = ref [];;
(* val c : '_a list ref = {contents = []} *)

c := 1 :: !c;;
(* - : unit = () *)

c;;
(* - : int list ref = {contents = [1]} *)
!c;;
(* - : int list = [1]  *)

(* c := true :: !c;;  *)
(* Error: This expression has type bool but an expression was expected of type int  *)

type seasons = [ `Spring | `Summer | `Autumn | `Winter ];;
(* type seasons = [ `Autumn | `Sprint | `Summer | `Winter ] *)

(* type seasons = [> `Spring | `Summer | `Autumn | `Winter ];; *)
(* Error: A type variable is unbound in this type declaration.
 * In type [> `Autumn | `Sprint | `Summer | `Winter ] as 'a
 * the variable 'a is unbound *)

type seasons_of_japan = [ seasons | `Tsuyu ];;
(* type seasons_of_japan = [ `Autumn | `Sprint | `Summer | `Tsuyu | `Winter ]  *)

(*************** 14.2 関数定義の拡張 ********************)

(* ---------- 14.2.1 差分の記述による関数定義 ---------------- *)

let hito' = function
    `Forward -> "walking forward"
    | `Backward -> "walking backward"
    | (`Right | `Left) as x -> kani x;;
(* val hito' : [< `Backward | `Forward | `Left | `Right ] -> string = <fun> *)

hito' `Right;;
(* - : string = "walking to the right" *)

(*
let hito' = function
    `Forward -> "walking forward"
    | `Backward -> "walking backward"
    | x -> kani x;;
*)
(* Error: This expression has type [> `Backward | `Forward ]
       but an expression was expected of type [< `Left | `Right ]
       The second variant type does not allow tag(s) `Backward, `Forward *)

(* --------- 14.2.2 #パターン ---------------- *)

type kani_dir = [`Left | `Right];;
(* type kani_dir = [ `Left | `Right ] *)

let hito' = function
    `Forward -> "walking forward"
    | `Backward -> "walking backward"
    | #kani_dir as x -> kani x;;
(* val hito' : [< `Backward | `Forward | `Left | `Right ] -> string = <fun> *)


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

let rec max_list = function
    `Cons(x, `Nil) -> x
    | `Cons(x, `Cons(y, l)) ->
            if x < y then max_list (`Cons(y, l))
            else max_list (`Cons(x, l));;
(* val max_list : [ `Cons of 'a * ([< `Cons of 'a * 'b | `Nil ] as 'b) ] -> 'a =
  <fun> *)

let rec max_list = function
    `Cons (x, `Nil) -> x
    | `Cons (x, (`Cons (_, _) as l)) ->
            let m = max_list l in 
            if x > m then x else m;;
(* val max_list : [ `Cons of 'a * ([< `Cons of 'a * 'b | `Nil ] as 'b) ] -> 'a =
  <fun> *)

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

(******************* 14.4 その他 ***************)

(* --------- 14.4.1 型付けに関する注意 ----------- *)

(* function `A x -> x+1 | `A y -> (int_of_float y) + 2 | `B -> 2;; *)
(* Error: This expression has type int but an expression was expected of type float  *)

let f = function `A x -> x+1 | `B -> 2;;
(* val f : [< `A of int | `B ] -> int = <fun>  *)

let g = function `A y -> int_of_float y + 2 | `B -> 3;;
(* val g : [< `A of float | `B ] -> int = <fun>  *)

let f_or_g  = fun x -> if x then f else g;;
(* val f_or_g : bool -> [< `A of int & float | `B ] -> int = <fun> *)

let next_season = function
    `Spring -> `Summer | `Summer -> `Autumn | `Autumn -> `Winter | `Winter -> `Spring;;
(* val next_season :
 *   [< `Autum | `Spring | `Summer ] -> [> `Autumn | `Summer | `Winter ] = <fun> *)

next_season `Autumn;;
next_season `Winter;;

(**************** 多相的オブジェクトと多相ヴァリアント ****************)

let f o = [o#foo 1; o#bar "0xAB"];;
(* val f : < bar : string -> 'a; foo : int -> 'a; .. > -> 'a list = <fun> *)

let g v = match v with `Foo x -> x + 1 | `Bar y -> int_of_string y;;
(* val g : [< `Bar of string | `Foo of int ] -> int = <fun>  *)

let l = [`Foo 1; `Bar "0xAB"];;
(* val l : [> `Bar of string | `Foo of int ] list = [`Foo 1; `Bar "0xAB"] *)

List.map g l;;  (* - : int list = [2; 171] *)

f (object
  method foo x = x + 1
  method bar y = int_of_string y
end);;
(* - : int list = [2; 171] *)


