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
    
# rtree_of_tree (tree_of_rtree tree);;
-: sring rosetree =
    RBr ("a", [RBr ("b", [RBr ("c", [RLf]); RLf; RBr ("d", [RLf])]); RBr ("e", [RLf]); RBr ("f", [RLf])])
*)

(* 二分木 *)

(* 定義 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

(* 完全木 *)
let comptree = Br(1, Br(2, Br(4, Lf, Lf),
Br(5, Lf, Lf)), Br(3, Br(6, Lf, Lf),
Br(7, Lf, Lf)));;


let mytree = Br('a', Br('b', Lf, Lf), Br('c', Br('d', Lf, Lf), Br('e', Lf, Lf)));;

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

