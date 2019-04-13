(* プログラミング in OCaml 練習問題 11.2 *)

(* 練習問題 11.2 ファンクターをつくる *)

(*
MakeAbstractSet の機能を拡張して、下に示すシグネチャ SET の各関数をコメントに従って定義しなさい。また、整数の集合モジュールをファンクター適用によって生成して、各関数がうまく機能していることも（できる限り多くの例を用いて）示しなさい。

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
  end;;
 *)

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

module MyIntSet = MakeAbstractSet (struct
                                    type t = int
                                    let compare i j = i - j
                                  end);;
  
open MyIntSet;;

let list1 = add 5 (add 4 (add 3 (add 2 (add 1 empty))));;
let list2 = add 10 (add 8 (add 6 (add 4 (add 2 empty))));;
let list3 = empty;;

let test_empty1 = is_empty list1 = false;;
let test_empty2 = is_empty list3 = true;;

let test_mem1 = mem 3 list1 = true;;
let test_mem2 = mem 3 list2 = false;;
  
let test_inter = elements (inter list1 list2) = [2; 4];;
  
let test_union = elements (union list1 list2) = [1; 2; 3; 4; 5; 6; 8; 10];;

let test_diff = elements (diff list1 list2) = [1; 3; 5; 6; 8; 10];;
  
