(* 練習問題 6.12 *)

(*
格納された要素が 1, 2, 3, 4 になるよう二分木探索木の形をすべて列挙し、それぞれの形を作るためには空の木から初めて、どの順番で要素を add していけばよいか示しなさい。
*)

(* 二分木 *)
(* 定義 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;


(* 要素を木に追加する *)
let rec add t x =
    match t with
    Lf -> Br (x, Lf, Lf)
    | (Br (y, left, right) as whole) when x = y -> whole
    | Br (y, left, right) when x < y -> Br (y, add left x, right)
    | Br (y, left, right) -> Br (y, left, add right x);;

let mytree = Lf;;

(* 要素をリストにして、それを順に木に追加する。
    t -- tree, l -- list   *)
let rec addx t l =
    match l with
      [] -> t
    | v :: rest ->
       addx (add t v) rest;;

let test1 = addx mytree [3; 1; 2; 4] = Br (3, Br (1, Lf, Br (2, Lf, Lf)), Br (4, Lf, Lf));;
let test2 = addx mytree [3; 2; 1; 4] = Br (3, Br (2, Br (1, Lf, Lf), Lf), Br (4, Lf, Lf));;
let test3 = addx mytree [2; 1; 3; 4] = Br (2, Br (1, Lf, Lf), Br (3, Lf, Br (4, Lf, Lf)));;
let test4 = addx mytree [2; 4; 3; 1] = Br (2, Br (1, Lf, Lf), Br (4, Br (3, Lf, Lf), Lf));;
