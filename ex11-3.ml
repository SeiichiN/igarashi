(* プログラミング in OCaml 練習問題 11.3 *)

(* 練習問題 11.3 シグネチャの with なし *)

(*
MakeAbstractSet を次のように、ファンクターの返すモジュールのシグネチャに with をつけずに SET だけとして定義するとどんな不都合が発生するでしょうか？

module MakeAbstractSet (Ord : OrderedType) : SET =
  struct
    type elt = Ord.t
    type t = elt list
    let tmpty = []

    (* 以下省略 *)
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

module MakeAbstractSet (Ord : OrderedType) : SET
  =
    struct
      type elt = Ord.t
      type t = elt list

      let empty = []

      let is_empty = function
          [] -> true
        | _ -> false

      let rec mem elt = function
          [] -> false
        | x :: rest ->
           let r = Ord.compare elt x in
           (r = 0) || ((r > 0) && mem elt rest)

      let rec add elt = function
          [] -> [elt]
        | (x :: rest as s) ->
           match Ord.compare elt x with
             0 -> s
           | r when r < 0 -> elt :: s
           | _ -> x :: (add elt rest)

      let rec inter s1 s2 =
        match (s1, s2) with
          (s1, []) -> []
        | ([], s2) -> []
        | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
           match Ord.compare e1 e2 with
             0 -> e1 :: inter rest1 rest2
           | r when r < 0 -> inter rest1 s2
           | _ -> inter s1 rest2

      let rec union s1 s2 =
        match (s1, s2) with
          (s1, []) -> s1
        | ([], s2) -> s2
        | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
           match Ord.compare e1 e2 with
             0 -> e1 :: union rest1 rest2
           | r when r < 0 -> e1 :: union rest1 s2
           | _ -> e2 :: union s1 rest2

      let rec diff s1 s2 =
        match (s1, s2) with
          (s1, []) -> s1
        | ([], s2) -> s2
        | ((e1 :: rest1 as s1), (e2 :: rest2 as s2)) ->
           match Ord.compare e1 e2 with
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

let list1 = empty;;

let list1 = add 5 (add 4 (add 3 (add 2 (add 1 list1))));;

(*
  解答

上記の式 5 のところでエラーが出る。

Error: This expression has type int but an expression was expected of type elt

シグネチャ SET の定義のところでは

module type SET =
  sig
    type elt
    type t

となっているので、ファンクタに与えられたモジュールの要素の型は、シグネチャとしては未指定のままである。
だから、整数を Ord.t として受け付けないのである。
 *)
