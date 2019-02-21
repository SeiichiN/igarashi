(* 練習問題 3-14 *)
(*
関数 f の 区間 a .. b 定積分を近似計算する関数を定義せよ。
integral f a b

近似計算 -- 台形近似をつかう
a から b の区間を n 分割した、その1つの区間の長さを s とすると、
i 番目の台形の面積は
( f(a + (i-1)s) + f(a + is) ) * s / 2

 *)
let abs (x:float) =
  if x < 0. then -1. *. x
  else x;;

let n = 10;;

let daikei (a, b, h) =
  (a +. b) *. h /. 2.;;

let test3 = daikei (2., 3., 4.) = 10.;;
  
let square x = x *. x;;

let test4 = square 3. = 9.;;

let rec sum_of (f, n) =
  if n = 0 then 0.
  else
    sum_of (f, (n-1)) +. f ;;

let test5 = sum_of ((square 2.), 3) = 12.;;

let integral f a b =
  let n = 100 in       (* 区間を100分割する *)
  let abs (x:float) = if x < 0. then -1. *. x else x in
    (* 100分割したうちの1区間の長さを求める *)
  let s = abs(a -. b) /. (float_of_int n) in
  let daikei (a, b, h) = (a +. b) *. h /. 2. in  (* 台形の面積 *)
  let rec keisan i =
    if i > n then 0.
    else
      let left = f (a +. float_of_int(i-1) *. s) in
      let right = f (a +. (float_of_int i) *. s) in
      keisan (i+1) +. daikei (left, right, s)
  in
  keisan 1;;

let test6 = integral square 1.0 2.0 = 2.5;;  
