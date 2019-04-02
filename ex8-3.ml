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

let f = ref (fun y -> y + 1);;
   (* f -- 引数を 1 増加させて返すという関数が書き換え可能な参照として定義されている。
    # !f 5;; とすると、6 が返ってくる。
    しかし、この f 関数は、下の funny_fact の定義のあと、別の関数がセットされるので、（funny_factがセットされるので、別に (fun y -> y) という恒等関数でもかまわないはず。
    * *)

let funny_fact x =
  if x = 1 then 1
  else x * (!f (x -1));;
   (* !f で、(x-1) を引数として、f にセットされた関数を実行する。 *)

f := funny_fact;;
   (* f に、関数 funny_fact がセットされている。これにより、funny_fact の中の f! は、funny_fact ということになる。 *)

funny_fact 5;;

