(* 二分木 *)

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

(* 左の枝の深さ *)
let rec depth_l = function
    Lf -> 0
    | Br(_, left, right) ->
            1 + (depth_l left);;

(* 右の枝の深さ *)
let rec depth_r = function
    Lf -> 0
    | Br (_, left, right) ->
            1 + (depth_r right);;

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

(* 二分探索木 *)
(* ある要素が木の中にあるかどうかを問い合わせる *)
let rec mem t x =
    match t with
    Lf -> false
    | Br (y, left, right) ->
            if x = y then true
            else
                if x < y then mem left x
                else mem right x;;

(* 要素を木に追加する *)
let rec add t x =
    match t with
    Lf -> Br (x, Lf, Lf)
    | (Br (y, left, right) as whole) when x = y -> whole
    | Br (y, left, right) when x < y -> Br (y, add left x, right)
    | Br (y, left, right) -> Br (y, left, add right x);;

(* バラの木 *)
type 'a rosetree = RLf | RBr of 'a * 'a rosetree list;;

(* XML *)
(* 葉の部分にもデータを格納できるようにする *)
(*
 * 'a -- タグを表現するデータの型
 * 'b -- 葉に格納するデータの型
 *)
type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list;;

(* アドレス帳 *)
let addressbook =
    XBr ("addressbook", [
        XBr ("person", [
            XBr ("name", [XLf (Some "Atsushi Igarashi")]);
            XBr ("tel", [XLf (Some "075-123-4567")])]);
        XBr ("person", [XLf None]);
        XBr ("person", [XLf None])]);;

(* 文字列に変換する *)
let rec string_of_xml = function
    XBr (tag, xml_list) -> "<" ^ tag ^ ">" ^
    string_of_xmllist xml_list ^
    "</" ^ tag ^ ">"
    | XLf None -> ""
    | XLf (Some s) -> s
and string_of_xmllist = function
    [] -> ""
    | xml :: rest -> string_of_xml xml ^ string_of_xmllist rest;;

