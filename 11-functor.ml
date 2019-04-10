(* ファンクター *)

module IntSet = Set.Make(struct
    type t = int
    let compare i j = i - j
end);;

open IntSet;;

(*
module IntSet :
    sig
        type elt = int
        type t
        val empty : t
        val is_empty : t -> bool                                                             
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val diff : t -> t -> t
        val compare : t -> t -> elt
        val equal : t -> t -> bool
        val subset : t -> t -> bool
        val iter : (elt -> unit) -> t -> unit
        val map : (elt -> elt) -> t -> t
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val filter : (elt -> bool) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val cardinal : t -> elt
        val elements : t -> elt list
        val min_elt : t -> elt
        val min_elt_opt : t -> elt option
        val max_elt : t -> elt
        val max_elt_opt : t -> elt option
        val choose : t -> elt
        val choose_opt : t -> elt option
        val split : elt -> t -> t * bool * t
        val find : elt -> t -> elt
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val of_list : elt list -> t
    end
*)

let s1 = add 2 (add 1 empty)
(* val s1 : t = <abstr> *)

and s2 = add 1 (add 3 empty);;
(* val s2 : t = <abstr> *)

(* mem -- ある要素が集合に属するかどうか *)
(mem 1 s1, mem 2 s1, mem 3 s1);;
(* - : bool * bool * bool = (true, true, false) *)

(* inter -- 集合の共通(intersection)部分をとる *)
let s3 = inter s1 s2 in (mem 1 s3, mem 2 s3, mem 3 s3);;
(* - : bool * bool * bool = (true, false, false) *)

(* elements -- 要素をリストとして列挙する *)
let test_elements1 = elements s1 = [1; 2];;
let test_elements2 = elements s2 = [1; 3];;

(* ファンクター定義 *)
module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end;;
(* module type OrderedType = sig type t val compare : t -> t -> int end *)

module MakeSet (Order : OrderedType) =
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
  end;;
(*
module MakeSet :
     functor (Order : OrderedType) ->
       sig
         type elt = Order.t
         type t = elt list
         val empty : 'a list
         val mem : elt -> elt list -> bool
         val add : elt -> elt list -> elt list
         val inter : elt list -> elt list -> elt list
         val elements : 'a -> 'a
       end
*)

module MyIntSet = MakeSet (struct
    type t = int
    let compare i j = i - j
  end);;
(*
module MyIntSet :
     sig
       type elt = int
       type t = elt list
       val empty : 'a list
       val mem : elt -> elt list -> bool
       val add : elt -> elt list -> elt list
       val inter : elt list -> elt list -> elt list
       val elements : 'a -> 'a
     end
*)

open MyIntSet;;

let s1 = add 2 (add 1 empty)
(* val s1 : int list = [1; 2] *)

and s2 = add 1 (add 3 empty);;
(* val s2 :  int list = [1; 3] *)

(* mem -- ある要素が集合に属するかどうか *)
(mem 1 s1, mem 2 s1, mem 3 s1);;
(* - : bool * bool * bool = (true, true, false) *)

(* inter -- 集合の共通(intersection)部分をとる *)
let s3 = inter s1 s2 in (mem 1 s3, mem 2 s3, mem 3 s3);;
(* - : bool * bool * bool = (true, false, false) *)

(* elements -- 要素をリストとして列挙する *)
let test_elements1 = elements s1 = [1; 2];;
let test_elements2 = elements s2 = [1; 3];;

module MyIntSet' : sig
    type elt = int
    type t
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val inter : t -> t -> t
    val elements : t -> elt list
end

= MakeSet (struct
    type t = int
    let compare i j = i - j
  end);;

(* MyIntSet' を使ってみる *)
open MyIntSet';;

let s1 = add 2 (add 1 empty)
(* val s1 : int list = [1; 2] *)

and s2 = add 1 (add 3 empty);;
(* val s2 :  int list = [1; 3] *)

(* mem -- ある要素が集合に属するかどうか *)
(mem 1 s1, mem 2 s1, mem 3 s1);;
(* - : bool * bool * bool = (true, true, false) *)

(* inter -- 集合の共通(intersection)部分をとる *)
let s3 = inter s1 s2 in (mem 1 s3, mem 2 s3, mem 3 s3);;
(* - : bool * bool * bool = (true, false, false) *)

(* elements -- 要素をリストとして列挙する *)
let test_elements1 = elements s1 = [1; 2];;
let test_elements2 = elements s2 = [1; 3];;


(* 返り値のシグネチャを指定する *)
module MakeAbstractSet (Order : OrderedType) :
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
  end;;
(*
module MakeAbstractSet :
     functor (Order : OrderedType) ->
       sig
         type elt = Order.t
         type t
         val empty : t
         val mem : elt -> t -> bool
         val add : elt -> t -> t
         val inter : t -> t -> t
         val elements : t -> elt list
       end
*)

module AbstractIntSet = MakeAbstractSet (struct
    type t = int
    let compare i j = i - j
  end);;
(*
module AbstractIntSet :
  sig
    type elt = int
    type t
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val inter : t -> t -> t
    val elements : t -> elt list
  end
*)

(* AbstractIntSet を使ってみる *)
open AbstractIntSet;;

let s1 = add 2 (add 1 empty)
(* val s1 : AbstractIntSet.t = <abstr> *)

and s2 = add 1 (add 3 empty);;
(* val s2 : AbstractIntSet.t = <abstr> *)

(* mem -- ある要素が集合に属するかどうか *)
(mem 1 s1, mem 2 s1, mem 3 s1);;
(* - : bool * bool * bool = (true, true, false) *)

(* inter -- 集合の共通(intersection)部分をとる *)
let s3 = inter s1 s2 in (mem 1 s3, mem 2 s3, mem 3 s3);;
(* - : bool * bool * bool = (true, false, false) *)

(* elements -- 要素をリストとして列挙する *)
let test_elements1 = elements s1 = [1; 2];;
let test_elements2 = elements s2 = [1; 3];;
