(* 練習問題 6.2 *)

(*
 位置情報が付加された2つの図形が重なりを持つか判定する。
 figure with_location -> figure with_location -> bool
 型の関数 overlap を定義しなさい。ただし、位置は図形の中心を示し、正方形、長方形は、各辺がx軸・y軸に平行で、長方形に関しては、Rectangle(x, y) の x の表す辺がx軸に平行、と仮定します。
 *)


(* 図形をあらわすヴァリアント *)
type figure =
    Circle of int
    | Rectangle of int * int
    | Square of int;;

(* 位置情報を付加する *)
type 'a with_location = { loc_x: float; loc_y: float; body: 'a};;

(* 原点 *)
{ loc_x = 0.0; loc_y = 0.0; body = Point };;

(* ある長方形 *)
let rec1 = { loc_x = 4.; loc_y = 2.; body = Rectangle (4, 2) };;
let rec2 = { loc_x = 6.; loc_y = 3.; body = Rectangle (5, 3) };;

(* 自乗 *)
let joh (x: float) = x *. x;;
let test1 = joh 2. = 4.;;
let test2 = joh 3. = 9.;;

let abs (x: float) =
    if x < 0. then -1. *. x else x;;

let overlap a b =
  match (a.body, b.body) with
    Circle c1, Circle c2 ->
    let d1 = float_of_int c1 in
    let d2 = float_of_int c2 in
    if (joh(a.loc_x -. b.loc_x) +. joh(a.loc_y -. b.loc_y))
       < joh(d1 +. d2)
    then true
    else false
  | Rectangle (l1, l2), Rectangle (l3, l4) ->
     let d1 = float_of_int l1 in
     let d2 = float_of_int l2 in
     let d3 = float_of_int l3 in
     let d4 = float_of_int l4 in
     if (abs(a.loc_x -. b.loc_x) < (d1 +. d3) /. 2.) &&
          (abs(a.loc_y -. b.loc_y) < (d2 +. d4) /. 2.) 
     then true
     else false
  | Square l1, Square l2 ->
     let d1 = float_of_int l1 in
     let d2 = float_of_int l2 in
     if (abs(a.loc_x -. b.loc_x) < (d1 +. d2) /. 2.) &&
          (abs(a.loc_y -. b.loc_y) < (d1 +. d2) /. 2.)
     then true
     else false;;

(* すべての組み合わせを網羅するような関数を作りたかったが、よくわからなかった。
let overlap a b =
  match (a.body, b.body) with
    ((Square k1, (Circle k3 | Square k3 | Rectangle (l3, l4)))|
     (Circle k1, (Circle k3 | Square k3 | Rectangle (l3, l4)))|
     (Rectangle (l1, l2) (Circle k3 | Square k3 | Rectangle (l3, l4)))) ->
 *)    

let cir1 = { loc_x = 3.; loc_y = 3.; body = Circle 2 };;
let cir2 = { loc_x = 1.; loc_y = 1.; body = Circle 3 };;

let test3 = overlap cir1 cir2 = true;;

let cir3 = {loc_x = 5.; loc_y = 5.; body = Circle 1};;
let cir4 = {loc_x = 1.; loc_y = 1.; body = Circle 2};;

let test4 = overlap cir3 cir4 = false;;


