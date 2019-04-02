(* 練習問題 8.3 *)

(*
以下で定義する funny_fact は再帰的定義（rec）を使わずに階乗を計算しています。どのような仕組みで実現されているか説明しなさい。

   # let f = ref (fun y -> y + 1)
     let funny_fact x =
         if x = 1 then 1
         else x * (!f (x -1));;

   val f : (int -> int) ref = {contents = <fun>}
   val funny_fact : int -> int = <fun>
   
   # f := funny_fact;;
   - : unit = ()

   # funny_fact 5;;
   - : int = 120
 *)

let f = ref (fun y -> y + 1)
let funny_fact x =
  if x = 1 then 1
  else x * (!f (x -1));;

f := funny_fact;;

funny_fact 5;;

