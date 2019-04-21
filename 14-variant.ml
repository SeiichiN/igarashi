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

c := true :: !c;;
(* Error: This expression has type bool but an expression was expected of type int  *)

