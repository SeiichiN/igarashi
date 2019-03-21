(* 練習問題 6.12 *)

(*
格納された要素が 1, 2, 3, 4 になるよう二分木探索木の形をすべて列挙し、それぞれの形を作るためには空の木から初めて、どの順番で要素を add していけばよいか示しなさい。
*)

(* 二分木 *)

(* 定義 *)
type tree =
    Lf 
    | Br of {
        mutable v: int;
        mutable left: tree;
        mutable right: tree;
    };;

(*
let chartree = Br ('a', Br ('b', Br ('d', Lf, Lf), Lf), 
Br ('c', Br ('e', Lf, Lf), Br ('f', Lf, Lf)));;
*)

(* サイズ *)
let rec size = function
    Lf -> 0
    | Br {v; left; right} ->
            1 + size left + size right;;

(* 深さ *)
let rec depth = function
    Lf -> 0
    | Br {v; left; right} ->
            1 + max(depth left) (depth right);;

(* test *)
let testtree = Br{v = 1; left = Lf; right = Lf};;

(* 完全木 *)
let comptree = Br{1; Br{2; Br{4; Lf; Lf};
Br{5; Lf; Lf}}; Br{3: Br{6; Lf; Lf};
Br{7; Lf; Lf}}};;


(*
let mytree = Br('a', Br('b', Lf, Lf), Br('c', Br('d', Lf, Lf), Br('e', Lf, Lf)));;
*)

(* 要素を木に追加する *)
let rec add t x =
    match t with
    Lf -> Br (x, Lf, Lf)
    | (Br (y, left, right) as whole) when x = y -> whole
    | Br (y, left, right) when x < y -> Br (y, add left x, right)
    | Br (y, left, right) -> Br (y, left, add right x);;

let mytree = Lf;;

let addx t x =
    let newtree = add t x in
    t = newtree;;

