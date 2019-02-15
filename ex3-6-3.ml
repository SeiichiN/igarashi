(* 練習問題3.6 3 *)

let x = 5;;
let y = 3;;
let sum_and_diff = (x + y, x - y);;
let f(x, y) =
    let sum_and_diff = (x + y, x - y) in
    let (left, right) = sum_and_diff in
    (left - y, right - x + y + y);;
  
  
