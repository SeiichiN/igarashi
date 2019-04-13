module IntSet = Set.Make (struct
                           type t = int
                           let compare i j = i - j
                         end)
                            
(*
module IntSet :
  sig
    type elt = int
    type t
    val empty : t
 *)

let my = IntSet.add 2 (IntSet.add 1 IntSet.empty);;
  (* val my : IntSet.t = <abstr> *)

IntSet.elements my;;
  (*  - : int list = [1; 2] *)
    

module OrgInt =
  struct
    type t = int
    let compare i j = i - j
  end

module IntSet' = Set.Make (OrgInt);;


  (*
module OrgInt : sig type t = int val compare : t -> t -> t end
module IntSet :
  sig
    type elt = int
    type t = Set.Make(OrgInt).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
  ...
  end
   *)
  
let my2 = IntSet'.add 2 (IntSet'.add 1 IntSet'.empty);;
  (* val my2 : IntSet'.t = <abstr> *)

IntSet'.elements my2;;
  (*  - : int list = [1; 2] *)
