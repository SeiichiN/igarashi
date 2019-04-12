(* ファンクターをつくってみる *)
module type OrderedType =
    sig
        type t
        val compare : t -> t -> int
    end;;

module type SET =
    sig
        type elt
        type t
        val empty : t
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val inter : t -> t -> t
        val elements : t -> elt list
    end;;
  
  
  (*
MakeSet -- ファンクター
@param -- Order（仮引数） 引数としてモジュールをとる
          そのモジュールは、OrderedType というシグネチャ型が適用される
with -- シグネチャにパッチをあてて、型定義を補う 
   *)
module MakeSet (Order : OrderedType) : SET with type elt = Order.t =
    struct
        type elt = Order.t
        type t = elt list

        let empty = []

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
-> の左に Order が現れていて、シグネチャの中に、Order が現れている。
「これは、ファンクターが適用されたときに生成されるモジュールのシグネチャが、引数として与えられるストラクチャそのものに依存することをあらわしています。」
ファンクターのように、「返り値の型が引数の値に依存するような関数の方を依存型と呼ぶことがあります。」
*)

module MyIntSetModule =
    struct
        type t = int
        let compare i j = i - j
    end;;

module MyIntSet = MakeSet(
    struct
        type t = int
        let compare i j = i - j
    end
    );;

