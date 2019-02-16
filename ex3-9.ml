(* 練習問題 3.9 *)

let cond (b, e1, e2) :int = if b then e1 else e2;;

let rec fact n =
  cond ((n = 1),
        1,
        n * fact (n-1));;
(*
 cond 式のなかから、外側の fact式を呼んでいるから
うまくいかないのかな？
cond式の条件分岐がうまく働かず、n * fact(n-1)をずっとくりかえすため、
いつまでも終わらないので、Stack overflow となるのではないか。
参考：http://takatoh.hatenablog.com/entry/20071216/practice_3_9 
*)
  
let rec fact2 n =
  if n = 1
  then 1
  else n * fact2 (n-1);;
  
