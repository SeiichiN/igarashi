(* 復習 *)

let rec plus_square (n, a) =
    if n = 0
    then a
    else
        let a = a + n * n in
        plus_square (n - 1, a);;

let test1 = plus_square (3, 0) = 14;;

(* 1^2 + 2^2 + 3^2 + ... + n^2 *)
(* sum_of_square : int => int *)
let sum_of_square n =
    plus_square (n, 0);;

let test2 = sum_of_square 3 = 14;;
let test3 = sum_of_square 4 = 30;;


let rec plus_cube (n, a) =
    if n = 0 then a
    else
        let a = a + n * n * n in
        plus_cube (n - 1, a);;

let test4 = plus_cube (3, 0) = 36;;

let sum_of_cube n = plus_cube (n, 0);;

let test5 = sum_of_cube 3 = 36;;

(* 本のコード *)
let rec sum_of_square n =
    if n = 0 then 0
    else
        sum_of_square(n - 1) + n * n;;

let test11 = sum_of_square 3 = 14;;

let rec sum_of_cube n =
  if n = 0 then 0
  else
    sum_of_cube(n-1) + n * n * n;;

let test12 = sum_of_cube 3 = 36;;

(* sum_of_square関数と sum_of_cube関数の共通部分を関数にする *)
let rec sum_of (f, n) =
  if n = 0 then 0
  else
    sum_of (f, n-1) + f n;;

  (* sum_of_square関数 *)
let square x = x * x;;

let sum_of_square n = sum_of (square, n);;

let sum_of_cube n =
  let cube x = x * x * x in
  sum_of (cube, n);;

let test21 = sum_of_square 3 = 14;;
let test22 = sum_of_cube 3 = 36;;

  (************** 匿名関数 ****************)

let sum_of_cube n = sum_of((fun x -> x * x * x), n);;
  
let test31 = sum_of_cube 3 = 36;;

let sq5 = ((fun x -> x * x), 5) in
    sum_of sq5;;

(fun x -> x * x) 7;;

  (***************** カリー化と関数を返す関数 ********************)
    
let concat (s1, s2) = s1 ^ s2 ^ s1;;

let test31 = concat ("abc", "DEF") = "abcDEFabc";;

  (* concat をカリー化する *)
let concat_curry s1 = fun s2 -> s1 ^ s2 ^ s1;;

let test32 = (concat_curry "abc") "DEF" = "abcDEFabc";;

  (* sum_of関数をカリー化する *)
let rec sum_of f n =
  if n = 0 then 0 else f n + sum_of f (n-1);;

let test33 = sum_of square 3 = 14;;
  
let emphasize = concat_curry "_";;

let test34 = emphasize "Objective Caml" = "_Objective Caml_";;
  
