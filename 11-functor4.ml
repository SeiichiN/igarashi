(* ファンクターを自作する *)

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
*)

  (* ファンクターのストラクチャへの適用 *)
module MyIntSet = MakeSet (struct
                            type t = int
                            let compare i j = i - j
                           end)
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

let s1 = MyIntSet.add 2 (MyIntSet.add 1 MyIntSet.empty);;
  (* val s1 : int list = [1; 2] *)

MyIntSet.elements s1;;
  (* - : int list = [1; 2] *)                            

(* t の定義を隠すために、シグネチャを書き加える *)
module MakeSet' (Order : OrderedType) :
  (* elt list は隠す *)
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

   (* ファンクターのストラクチャへの適用 *)
module MyIntSet' = MakeSet' (struct
                            type t = int
                            let compare i j = i - j
                           end)
(*
module MyIntSet' :
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

let s2 = MyIntSet'.add 2 (MyIntSet'.add 1 MyIntSet'.empty);;
  (* val s2 : MyIntSet'.t = <abstr> *)

MyIntSet'.elements s2;;
  (* - : int list = [1; 2] *)                            

module type SET =
  sig
    type elt
    type t
    val empty : t
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val inter : t -> t -> t
    val elements : t -> elt list
  end

module MakeSet'' (Order : OrderedType) : SET with type elt = Order.t = 
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

   (* ファンクターのストラクチャへの適用 *)
module MyIntSet'' = MakeSet'' (struct
                                type t = int
                                let compare i j = i - j
                               end)

let s3 = MyIntSet''.add 2 (MyIntSet''.add 1 MyIntSet''.empty);;
  (* val s2 : MyIntSet''.t = <abstr> *)

MyIntSet''.elements s3;;
  (* - : int list = [1; 2] *)                            
