(* 練習問題 6.2 *)

(*
 位置情報が付加された2つの図形が重なりを持つか判定する。
 figure with_location -> figure with_location -> bool
 型の関数 overlap を定義しなさい。ただし、位置は図形の中心を示し、正方形、長方形は、各辺がx軸・y軸に平行で、長方形に関しては、Rectangle(x, y) の x の表す辺がx軸に平行、と仮定します。
 *)


(* 図形をあらわすヴァリアント *)
type figure =
    Point
    | Circle of int
    | Rectangle of int * int
    | Square of int;;

(* 位置情報を付加する *)
type 'a with_location = { loc_x: float; loc_y: float; body: 'a};;

(* 原点 *)
{ loc_x = 0.0; loc_y = 0.0; body = Point };;

(* ある長方形 *)
let rec1 = { loc_x = 4.; loc_y = 2.; body = Rectangle (4, 2) };;
let rec2 = { loc_x = 6.; loc_y = 3.; body = Rectangle (5, 3) };;

let overlap a b =
    match (a.body, b.body) with
    Point, Point -> true
    | Circle c1, Circle c2 ->
            ((a.loc_x -. b.loc_x) *. (a.loc_x -. b.loc_x) +.
            (a.loc_y -. b.loc_y) *. (a.loc_y -. b.loc_y) ) >
            (c1 + c2) * (c1 + c2);;


