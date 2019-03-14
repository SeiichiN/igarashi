(* 第6章 レコードとヴァリアント *)

(* 型宣言  student という型を宣言 *)
type student = {name : string; id : int};;

(* st1 という具体的なレコードを作成 *)
let st1 = {name = "Taro Yamada"; id = 123456 }

(* レコードを作成するときに、順番は関係ない。 *)
and st2 = {id = 51; name = "Ichiro Suzuki"};;

(* 一部のフィールドのみ値を変更したレコードを作成することも可能。 *)
let st3 = { st1 with id = 234567 };;

(* 同じフィールド名をもつ別の型を宣言すると、古いフィールド名は隠されてしまう。以下のようなことをすると、student型の nameフィールドの値にアクセスできなくなってしまう。 *)
(* type foo = {name : bool; };; *)

(* ============== 要素の取り出し方 ============ *)

(* {フィールド名 = パターン} での取り出し方 *)
let string_of_student {name = n; id = i} =
    n ^ "'s ID is " ^ string_of_int i;;


string_of_student st1;;

let name_of_student {name = n} = n ^ "君です。";;

name_of_student st2;;

(* .（ドット）を使った取り出し方 *)
let string_of_student st = st.name ^ "'s ID is " ^ string_of_int st.id;;

string_of_student st3;;

(* 入れ子になったレコードの例 *)
type teacher = {tname : string;
                office : string;    (* 居室 *)
                ext    : int} ;;    (* 内線 *)

type student_teacher = {s : student; t : teacher} ;;

let () = print_string "============================================\n";;

(* ================ ヴァリアント ============== *)

(* 図形をあらわすヴァリアント *)
type figure =
    Point
    | Circle of int
    | Rectangle of int * int
    | Square of int;;

let c = Circle 3;;

let figs = [Point; Square 5; Rectangle (4, 5); c];;

let figs2 = [Rectangle (3, 6)];;

(* 面積を求める *)
let area_of_figure = function
    Point -> 0
    | Circle r -> r * r * 3
    | Rectangle (x, y) -> x * y
    | Square x -> x * x;;


let map f l =
    let rec map_in f l e =
        match l with 
    [] -> []
    | v :: rest ->
            (f v) :: map_in f rest e
    in
    map_in f l [];;

area_of_figure c;;

map area_of_figure figs;;

map area_of_figure figs2;;

(* 相似形 *)
let similar x y =
    match (x, y) with
    (Point, Point) | (Circle _, Circle _) | (Square _, Square _) ->true
    | (Rectangle (l1, l2), Rectangle (l3, l4)) ->
            (l3 * l2 - l4 * l1) = 0
    | _ -> false;;

similar (Rectangle (2, 4)) (Rectangle (1, 2));;

similar (Circle 3) (Circle 5);;

(* 列挙型 *)
type color = White | Black | Blue | Green | Red | Magenta | Yellow | Cyan;;

let reverse_color = function
    White -> Black | Black -> White | Blue -> Yellow | Yellow -> Blue | Red -> Cyan | Cyan -> Red | Magenta -> Green | Green -> Magenta;;

reverse_color Cyan;;

(* 再帰ヴァリアント型 *)

(* 自然数の定義 *)
(*
 * ゼロは自然数である。
 * 自然数に 1 足した数は自然数である。
 *)
type nat = Zero | OneMoreThan of nat;;

let zero = Zero
and two = OneMoreThan (OneMoreThan Zero);;

let rec add m n =
    match m with
    Zero -> n
    | OneMoreThan m' ->
            OneMoreThan (add m' n);;

(* 整数リスト *)
type intlist = INil | ICons of int * intlist;;


(* kisuu & guusuu *)
type even = Zero | OMT_E of odd (* OMT -- OneMoreThan *)
and odd = OMT_O of even;;

let rec o_plus_e (OMT_O e1) e2 = OMT_O (e_plus_e e1 e2) 
and e_plus_e e1 e2 =
    match e1 with Zero -> e2 | OMT_E o -> OMT_E (o_plus_e o e2);;

let one = OMT_O Zero;;
let two = OMT_E one;;
let three = OMT_O two;;
let four = OMT_E three;;

(* 多相的なリスト *)
type 'a mylist = Nil | Cons of 'a * 'a mylist;;

type 'a with_location = { loc_x: float; loc_y: float; body: 'a };;


