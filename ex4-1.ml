(* 練習問題 4.1 *)

let curry f x y = f (x, y);;
let average (x, y) = (x +. y) /. 2.0;;
let curried_avg = curry average;;

let test1 = average (4.0, 5.3) = 4.65;;
let test2 = curried_avg 4.0 5.3 = 4.65;;

let uncurry f (x, y) = f x y;;

let avg = uncurry curried_avg in
avg (4.0, 5.3);;

