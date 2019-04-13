(* プログラミング in OCaml 練習問題 11.1 *)

(* 練習問題 11.1 *)

(*
下の
module OrderedInt' : OrderedType =
  struct
    type t = int
    let compare i j = i - j
  end;;

module IntSet' = MakeSet (OrderedInt');;

のように、MakeSet に与える引数のシグネチャを OrderedType として明示的に指定した場合、このモジュール IntSet' は、IntSet と、どういった点で異なっているでしょうか。
 *)

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module MakeSet (Order : OrderedType) : 
    sig
      type elt = Order.t
      type t
      val empty : t
      val mem : elt -> t -> bool
      val add : elt -> t -> t
      val inter : t -> t -> t
      val elements : t -> elt list
    end
=
  struct
    type elt = Order.t
    type t = elt list
    
    let empty = []

    let rec mem elt = function
        [] -> false
      | x :: rest ->
        let r = Order.compare elt x in
        (r = 0) || ((r > 0) && mem elt rest)
    (* if elt = x then true else mem elt rest は、あかんの？*)

    let rec add elt = function
        [] -> [elt]
      | (x :: rest as s) ->
        match Order.compare elt x with
        0 -> s
        | r when r < 0 -> elt :: s
        | _ -> x :: (add elt rest)
    (* このリストは、昇順にならんでいるリストやな *)

    let rec inter s1 s2 =
      match (s1, s2) with
        (s1, []) -> []
      |([], s2) -> []
      |((e1::rest1 as s1), (e2::rest2 as s2)) ->
        match Order.compare e1 e2 with
        0 -> e1 :: inter rest1 rest2
        | r when r < 0 -> inter rest1 s2
        | _ -> inter s1 rest2

    let rec elements s = s
  end

(* 比較のために、シグネチャのないモジュールを用意した *)
module OrderedInt  =
  struct
    type t = int
    let compare i j = i - j
  end
(*
module OrderedInt : sig type t = int val compare : t -> t -> t end
 *)
  
module IntSet = MakeSet (OrderedInt);;

(*
module IntSet :
  sig
    type elt = int
    type t = MakeSet(OrderedInt).t
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val inter : t -> t -> t
    val elements : t -> elt list
  end
 *)
  
(* こちらは問題文に書かれているシグネチャつきのモジュールである *)  
module OrderedInt' : OrderedType  =
  struct
    type t = int
    let compare i j = i - j
  end
(*
module OrderedInt' : OrderedType
 *)

module IntSet' = MakeSet (OrderedInt');;
(*
module IntSet' :
  sig
    type elt = OrderedInt'.t
    type t = MakeSet(OrderedInt').t
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val inter : t -> t -> t
    val elements : t -> elt list
  end
 *)
                        
  
let s1 = IntSet.add 2 (IntSet.add 1 IntSet.empty);;
  (* val s1 : IntSet.t = <abstr> *)
IntSet.elements s1;;
  (* - : int list = [1; 2] *)

let s2 = IntSet'.add 2 (IntSet'.add 1 IntSet'.empty);;
  (* Error: This expression has type int but an expression was expected of type IntSet'.elt *)
  
(*
解答

OrderedInt' と、シグネチャつきのモジュールを引数として MakeSet に渡すと、type elt = OrderedInt'.t となる。

そののち、IntSet'.add 1 IntSet'.empty などとすると、型が合わないというエラーが出る。
Error: This expression has type int but an expression was expected of type IntSet'.elt

 *)
  
