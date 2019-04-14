(* プログラミング in OCaml 練習問題 11.4 *)

(* 練習問題 11.4 *)

(*
（NaiveSig が与えられていない生の）BadPair が、ファンクター MakeTest に渡せないことを確かめなさい。
 *)

(* 問題文の環境を以下のように想定する *)

module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module type SET =
  sig
    type elt
    type t
    val empty : t  (* 空集合 *)
    val is_empty : t -> bool  (* 集合か空かのテスト *)
    val mem : elt -> t -> bool  (* elt が t に属しているかのテスト *)
    val add : elt -> t -> t  (* 要素 elt を t に加えた集合を返す *)
    val inter : t -> t -> t  (* ふたつの集合の共通部分を返す *)
    val union : t -> t -> t  (* ふたつの集合の和集合を返す *)
    val diff : t -> t -> t   (* ふたつの集合の差を返す *)
    val elements : t -> elt list  (* 集合要素を昇順整列済リストとして返す *)
  end

module MakeAbstractSet (Order : OrderedType) : SET with type elt = Order.t
  =
    struct
      type elt = Order.t
      type t = elt list

      let empty = []

      let is_empty = function
          [] -> true
        | _ -> false

      let rec mem elt = function
          [] -> false
        | x :: rest ->
           let r = Order.compare elt x in
           (r = 0) || ((r > 0) && mem elt rest)

      let rec add elt = function
          [] -> [elt]
        | (x :: rest as s) ->
           match Order.compare elt x with
             0 -> s
           | r when r < 0 -> elt :: s
           | _ -> x :: (add elt rest)

      let rec inter s1 s2 =
        match (s1, s2) with
          (s1, []) -> []
        | ([], s2) -> []
        | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
           match Order.compare e1 e2 with
             0 -> e1 :: inter rest1 rest2
           | r when r < 0 -> inter rest1 s2
           | _ -> inter s1 rest2

      let rec union s1 s2 =
        match (s1, s2) with
          (s1, []) -> s1
        | ([], s2) -> s2
        | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
           match Order.compare e1 e2 with
             0 -> e1 :: union rest1 rest2
           | r when r < 0 -> e1 :: union rest1 s2
           | _ -> e2 :: union s1 rest2

      let rec diff s1 s2 =
        match (s1, s2) with
          (s1, []) -> s1
        | ([], s2) -> s2
        | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
           match Order.compare e1 e2 with
             0 -> diff rest1 rest2
           | r when r < 0 -> e1 :: diff rest1 s2
           | _ -> e2 :: diff s1 rest2
                        
      let rec elements s = s
    end

(* BadPair を以下のように定義する *)
      
module BadPair =
  struct
    module Elt =
      struct
        type t = int
        let compare i j = i - j
      end

    module Set = MakeAbstractSet(Elt)
  end;;

(* NaiveSig を以下のように定義する *)
  
module type NaiveSig =
  sig
    module Elt : OrderedType
    module Set : SET with type elt = Elt.t
  end;;

(* NaiveSig が与えられている場合 *)
  
module MakeTest (P : NaiveSig ) =
  struct
    let test_elements set =
      let rec loop = function
          [] | [_] -> true
          | x :: y :: rest ->
             if P.Elt.compare x y > 0 then false
             else loop (y :: rest)
      in
      loop (P.Set.elements set)
  end;;
  
module Test = MakeTest(BadPair);;

let list1 = BadPair.Set.add 2 (BadPair.Set.add 1 BadPair.Set.empty);;
let list2 = BadPair.Set.add 1 (BadPair.Set.add 2 BadPair.Set.empty);;
let list3 = [3; 2; 1];;
  
(* NaiveSig が与えられていない場合 *)
  
module MakeTest (P ) =
  struct
    let test_elements set =
      let rec loop = function
          [] | [_] -> true
          | x :: y :: rest ->
             if P.Elt.compare x y > 0 then false
             else loop (y :: rest)
      in
      loop (P.Set.elements set)
  end;;
  
module Test = MakeTest(BadPair);;

let list1 = BadPair.Set.add 2 (BadPair.Set.add 1 BadPair.Set.empty);;
let list2 = BadPair.Set.add 1 (BadPair.Set.add 2 BadPair.Set.empty);;
let list3 = [3; 2; 1];;
  
