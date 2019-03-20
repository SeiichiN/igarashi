(* 練習問題 6.11 *)

(*
上の arith から、その数式の文字列表現を求める関数 string_of_arith、分配則を用いて数式を (i11 X ... X i1n1_ + ... + (im1 X ... X imnm) （ただし ixy は整数）の形に変形する関数 expand を定義しなさい。

# string_of_arith exp;;
- : string = "((3 + 4) * (2 + 5))"
# string_of_arith (expand exp);;
- : string = "(((3 * 2) + (3 * 5)) + ((4 * 2) + (4 * 5)))"

簡単だと思う人は、string_of_arith の出力結果のカッコを減らすように工夫しなさい。
*)

type arith =
    Const of int |Add of arith * arith | Mul of arith * arith;;

let exp = Mul (Add ( Const 3, Const 4), Add ( Const 2, Const 5));;

let rec eval siki =
    match siki with
    Const v -> v
    | Add (v1, v2) -> eval v1 + eval v2
    | Mul (v1, v2) -> eval v1 * eval v2;;

let test1 = eval exp = 49;;

let rec string_of_arith e =
    match e with
    Const v -> string_of_int v
    | Add (v1, v2) -> "(" ^ string_of_arith v1 ^ "+" ^ string_of_arith v2 ^ ")"
    | Mul (v1, v2) -> "(" ^ string_of_arith v1 ^ "*" ^ string_of_arith v2 ^ ")";;

let expand e =
    match e with
    Const v -> Const v
    | Add (v1, v2) -> Add (v1, v2)
    | Mul (v1, v2) -> 
            match (v1, v2) with
            (Add(x1, x2), Add(y1, y2)) ->
                Add( Add (Mul (x1, y1), Mul (x1, y2)),
                     Add (Mul (x2, y1), Mul (x2, y2)));;

