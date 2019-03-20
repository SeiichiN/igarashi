(* 練習問題 6.8 *)

(*
二分木をバラの木に変換する関数 rtree_of_tree を、6.4節で紹介した tree_of_rtree の逆関数になるように定義しなさい。
*)

(*
# rtree_of_tree;;
- : 'a option tree -> 'a rosetree = <fun>
# rtree;;
- : string rosetree =
    RBr ("a", [RBr ("b", [RBr ("c", [RLf]); RLf; RBr ("d", [RLf])]); RBr ("e", [RLf]); RBr ("f", [RLf])])
    
# rtree_of_tree (tree_of_rtree rtree);;
-: sring rosetree =
    RBr ("a", [RBr ("b", [RBr ("c", [RLf]); RLf; RBr ("d", [RLf])]); RBr ("e", [RLf]); RBr ("f", [RLf])])
*)

(* 二分木 *)

(* 定義 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;


(* バラの木 *)
type 'a rosetree = RLf | RBr of 'a * 'a rosetree list;;

let rec map f = function
    [] -> []
  | v :: rest -> f v :: map f rest;;

(* 二分木をバラの木に変換する *)
let rec rosetree_of_tree = function
    Lf -> RLf
    | Br(a, left, right) ->
            RBr(a, map rosetree_of_tree [left; right]);;

(* バラの木を二分木に変換する *)
let rec tree_of_rtree = function
    RLf -> Br (None, Lf, Lf)
    | RBr (a, rtrees) -> Br (Some a, tree_of_rtreelist rtrees, Lf)
and tree_of_rtreelist = function
    [] -> Lf
    | rtree :: rest ->
            let Br (a, left, Lf) = tree_of_rtree rtree in
            Br (a, left, tree_of_rtreelist rest);;

(* 実行例 *)
let rtree =
    RBr ("a", [
        RBr ("b", [
            RBr ("c", [RLf]);
            RLf;
            RBr ("d", [RLf])]);
        RBr ("e", [RLf]);
        RBr ("f", [RLf])]);;

let thetree = 
    Br (Some "a",
     Br (Some "b",
      Br (Some "c", Br (None, Lf, Lf),
       Br (None, Lf, Br (Some "d", Br (None, Lf, Lf), Lf))),
      Br (Some "e", Br (None, Lf, Lf),
       Br (Some "f", Br (None, Lf, Lf), Lf))),
     Lf);;

(* 解答 *)
let rec rtree_of_tree = function
    Lf -> RLf
    | Br (None, left, right) ->
            RBr (None, map rtree_of_tree [left; right])
    | Br (Some a, left, right) ->
            RBr (Some a, map rtree_of_tree [left; right]);;

let rtree = 
    RBr ("a", [RBr ("b", [RBr ("c", [RLf]); RLf; RBr ("d", [RLf])]); RBr ("e", [RLf]); RBr ("f", [RLf])]);;
    
let test1 = rtree_of_tree (tree_of_rtree rtree) = 
    RBr (Some "a",
 [RBr (Some "b",
   [RBr (Some "c",
      [RBr (None, [RLf; RLf]);
            RBr (None,
                   [RLf; RBr (Some "d", [RBr (None, [RLf; RLf]); RLf])])]);
                       RBr (Some "e",
                            [RBr (None, [RLf; RLf]);
                                  RBr (Some "f", [RBr (None, [RLf; RLf]); RLf])])]);
                                    RLf])
