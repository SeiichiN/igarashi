(* 練習問題 3.8 *)
(* 指数関数を iterpow という名前で反復的に定義しなさい *)

let rec iterpow (n, x) =
  if x = 0 then 1.
  else
    if x > 0
    then iterpow (n, x - 1) *. n
    else iterpow (n, x + 1) /. n;;
  

  (* TEST *)
let test1 = iterpow(2., 3) = 8.;;
let test2 = iterpow(2., 4) = 16.;;
let test3 = iterpow(0.5, 1) = 0.5;;
let test4 = iterpow(0.5, 2) = 0.25;;
let test5 = iterpow(2., -1) = 0.5;;
let test6 = iterpow(0.5, -1) = 2.0;;
let test7 = iterpow(0.5, -2) = 4.0;;
            
