(* 練習問題 6.6 *)

(*
 * preord と同様な方法で、通りがけ順、帰りがけ順に列挙する関数 inord, postord を定義しなさい。
 *)

(* 定義 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let chartree = Br ('a', Br ('b', Br ('d', Lf, Lf), Lf), 
Br ('c', Br ('e', Lf, Lf), Br ('f', Lf, Lf)));;

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

(* @ を使わずに・・・ *)
let rec preord t l =
    match t with
    Lf -> l
    | Br (x, left, right) ->
            x :: (preord left (preord right l));;

(* 解答 *)
(* 通りがけ順 *)
let rec inord t l =
    match t with
    Lf -> l
    | Br (x, left, right) ->
            inord left (x :: (inord right l));;

(* 帰りがけ順 *)
let rec postord t l =
    match t with
    Lf -> l
    | Br (x, left, right) ->
            (postord left (postord right (x :: l)));;

