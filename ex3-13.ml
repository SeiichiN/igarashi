(* 練習問題 3-13 *)

(* 練習問題 3-7で定義したpow関数をカリー化せよ。 *)

(*
 * pow; x の n乗 を返す
 * pow: float * int -> float
 * @param: float x -- 底となる数
 *         int   n -- 何乗か？
 *)
let rec pow (x, n) =
  if n = 1 then x
  else
    pow(x, n-1) *. x;;

let test3 = pow(2., 3) = 8.;;
let test4 = pow(3., 3) = 27.;;
  
(* カリー化関数 *)
let rec pow x = (fun n ->
    if n = 1 then x
    else
        (pow x (n-1)) *. x);;

let test5 = pow 2. 3 = 8.;;

(* 第1引数が n となるようにする *)
let rec pow n = (fun x ->
    if n = 1 then x
    else
        (pow (n-1) x) *. x);;

let test6 = pow 3 2. = 8.;;

(* cube *)
let rec cube n = (fun x ->
    if n = 1 then x
    else
        (cube (n-1) x) *. x );;

let test7 = cube 3 2. = 8.;;

let cube x = pow 3 x;;

let test8 = cube 3. = 27.;;


let rec pow x = (fun n ->
    if n = 1 then x
    else
        (pow x (n-1)) *. x);;

let cube x = pow x 3;;

let test9 = cube 3. = 27.;;

