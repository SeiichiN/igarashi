(* 目的：身長と体重を与えられたら、BMI指数を返す *)
(* bmi: float -> float -> float *)
let bmi m kg = floor((( kg /. m ** 2.) *. 100.) +. 0.5) /. 100.

    (* テスト *)
let test1 = bmi 1.7 70. = 24.22
    
