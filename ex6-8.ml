(* 練習問題 6.8 *)

(*
二分木をバラの木に変換する関数 rtree_of_tree を、6.4節で紹介した tree_of_rtree の逆関数になるように定義しなさい。
*)


# rtree_of_tree;;
- : 'a option tree -> 'a rosetree = <fun>
# rtree;;
- : string rosetree =
    RBr ("a", [RBr ("b", [RBr ("c", [RLf]); RLf; RBr ("d", [RLf])]); RBr ("e", [RLf]); RBr ("f", [RLf])])
    
# rtree_of_tree (tree_of_rtree tree);;
-: sring rosetree =
    RBr ("a", [RBr ("b", [RBr ("c", [RLf]); RLf; RBr ("d", [RLf])]); RBr ("e", [RLf]); RBr ("f", [RLf])])

