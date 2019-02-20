(* 練習問題 3-14 *)
(*
関数 f の 区間 a .. b 定積分を近似計算する関数を定義せよ。
integral f a b

近似計算 -- 台形近似をつかう
a から b の区間を n 分割した、その1つの区間の長さを s とすると、
i 番目の台形の面積は
( f(a + (i-1)s) + f(a + is) ) * s / 2

 *)
let abs x =
  if x < 0 then -x
  else x;;
    
let integral f =
  (fun a ->
   (fun b ->
    let s = abs(a - b) / n in
    let x = ((f (a + (i-1)*s)) + (f (a + i*s))) * s / 2 in
   ) );;
