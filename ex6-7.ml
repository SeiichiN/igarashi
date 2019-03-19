(* 練習問題 6.7 *)

(*
 * 二分木の左右を反転させた木を返す関数 reflect を定義しなさい。
 *
 * # reflect comptree;;
 * - : int tree = 
 * Br(1, Br(3, Br(7, Lf, Lf), Br(6, Lf, Lf)),
 * Br(2, Br(5, Lf, Lf), Br(4, Lf, Lf)))
 *
 * また、任意の二分木 t に対して成立する、次の方程式を完成させなさい。
 * preorder(reflect(t)) = ?  reverse (postorder t)
 * inorder(reflect(t)) = ? reverse (inorder t)
 * postorder(reflect(t)) = ? reverse (preorder t)
 *)

(* 二分木 *)

(* 定義 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

(* サイズ *)
let rec size = function
    Lf -> 0
    | Br (_, left, right) ->
            1 + size left + size right;;

(* 深さ *)
let rec depth = function
    Lf -> 0
    | Br (_, left, right) ->
            1 + max(depth left) (depth right);;

(* 完全木 *)
let comptree = Br(1, Br(2, Br(4, Lf, Lf),
Br(5, Lf, Lf)), Br(3, Br(6, Lf, Lf),
Br(7, Lf, Lf)));;


let mytree = Br('a', Br('b', Lf, Lf), Br('c', Br('d', Lf, Lf), Br('e', Lf, Lf)));;

(* 解答 *)
let rec reflect t =
    match t with
    Lf -> Lf
    | Br (x, left, right) ->
            Br (x, (reflect right), (reflect left));;


(* 行きがけ順 -- preorder *)
let rec preorder = function
    Lf -> []
    | Br (x, left, right) -> 
            x :: (preorder left) @ (preorder right);;

(* 通りがけ順 -- inorder *)
let rec inorder = function
    Lf -> []
    | Br (x, left, right) ->
            (inorder left) @ (x :: inorder right);;

(* 帰りがけ順 -- postorder *)
let rec postorder = function
    Lf -> []
    | Br (x, left, right) ->
            (postorder left) @ (postorder right) @ [x];;

(* reverse 関数 *)
let rec reverse = function
    [] -> []
  | v :: rest -> reverse rest @ [v];;

let t = comptree;;

let test1 = preorder(reflect(t)) = reverse (postorder t);;
let test2 = inorder(reflect(t)) = reverse (inorder t) ;;
let test3 = postorder(reflect(t)) = reverse (preorder t);;
