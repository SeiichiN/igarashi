(* 練習問題 6.1 *)

(* 関数 similar を、2つの辺の長さが等しい長方形は正方形と相似だと判定するように改造しなさい。 *)

(* 図形を表すヴァリアント *)
type figure =
    Point
    | Circle of int
    | Rectangle of int * int
    | Square of int * int;;

let similar x y =
    match (x, y) with
    (Point, Point)
    | (Circle _, Circle _) -> true
    | (Square (l1, l2), Square (l3, l4)) -> 
            (l1 - l2) = 0 && (l3 - l4) = 0
    | (Rectangle (l1, l2), Rectangle (l3, l4)) ->
            (l3 * l2 - l4 * l1) = 0
    | _ -> false;;

similar (Rectangle (2, 4)) (Rectangle (1, 2));;

similar (Circle 3) (Circle 5);;

            
