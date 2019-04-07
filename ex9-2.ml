(* プログラミング in OCaml 練習問題 9.2 *)

(* 練習問題 9.2 *)

(*
二分探索木を使ったテーブルを、シグネチャとして TABLE2 を与えたモジュールとして実装しなさい。そして、各関数が機能していることを確かめなさい。
 *)

module Tree =
    struct
        type ('a, 'b) t = Lf | Br of 'a * 'b * ('a, 'b) t * ('a, 'b) t

        let empty = Lf

        let rec size = function
            Lf -> 0
            | Br (_, _,  left, right) -> 1 + size left + size right

        let rec depth = function
            Lf -> 0
            | Br (_, _, left, right) -> 1 + max (depth left) (depth right)

        let rec add key datum t = 
            match t with
            Lf -> Br(key, datum, Lf, Lf)
            | (Br (key', datum', left, right) as whole) when key = key' -> whole
            | Br (key', datum', left, right) when key < key' -> Br (key', datum', add key datum left, right)
            | Br (key', datum', left, right) -> Br (key', datum', left, add key datum right)

        let rec retrieve key = function
            Lf -> None
            | Br (key', datum, left, right) ->
                    if key = key' then Some datum
                    else
                        if key < key'
                        then retrieve key left
                        else retrieve key right
        
        let rec dump = function
            Lf -> []
            | Br (key, datum, left, right) ->
                    (key, datum) :: (dump left) @ (dump right)
    end;;

module type TABLE2 =
    sig
        type ('a, 'b) t

        val empty : ('a, 'b) t

        val size : ('a, 'b) t -> int

        val depth : ('a, 'b) t -> int

        val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

        val retrieve : 'a -> ('a, 'b) t -> 'b option

        val dump : ('a, 'b) t -> ('a * 'b) list
        
    end;;
        
module AbsTree : TABLE2 = Tree;;


let tr = AbsTree.empty;;
(* val tr : ('a, 'b) AbsTree.t = <abstr> *)

let tr = AbsTree.add "l" "love" tr;;
(* val tr : (string, string) AbsTree.t = <abstr> *)

let tr = AbsTree.add "p" "pineapple" tr;;
(* val tr : (string, string) AbsTree.t = <abstr> *)

AbsTree.size tr;;  (* - : int = 2 *)
AbsTree.depth tr;;  (* - : int = 2 *)

let tr = AbsTree.add "a" "apple" tr;;
(* val tr : (string, string) AbsTree.t = <abstr> *)

AbsTree.retrieve "p" tr;;
(* - : string option = Some "pineapple" *)

AbsTree.dump tr;;
(* - : (string * string) list =
  [("l", "love"); ("a", "apple"); ("p", "pineapple")] *)

