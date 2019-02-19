(* 復習 *)

let rec plus_square (n, a) =
    if n = 1
    then a
    else
        let a = a + n * n in
        plus_square (n - 1, a);;

let test1 = plus_square (3, 0) = 13;;

(* 1^2 + 2^2 + 3^2 + ... + n^2 *)
(* sum_of_square : int => int *)
let sum_of_square n =
    plus_square (n, 0);;

let test2 = sum_of_square 3 = 13;;
let test3 = sum_of_square 4 = 29;;


let rec plus_cube (n, a) =
    if n = 1 then a
    else
        let a = a + n * n * n in
        plus_cube (n - 1, a);;

let test4 = plus_cube (3, 0) = 35;;

let sum_of_cube n = plus_cube (n, 0);;

let test5 = sum_of_cube 3 = 35;;

(*  *)
let rec sum_of_square2 n =
    if n = 0 then 0
    else
        sum_of_square2(n - 1) + n * n;;

let test11 = sum_of_square2 3 = 13;;

