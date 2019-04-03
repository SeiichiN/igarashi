(* 練習問題 8.4 *)

(*
参照と繰り返しの構文 (while, for)を使って、フィボナッチ数を求める関数を定義しなさい。
 *)

let fib n =
  if (n < 3) then 1
  else
    let mem1 = ref 1;
    and mem2 = ref 1;
    and mem3 = ref 1
    in
    let x = ref 3 in
    while (!x <= n) do
      mem1 := !mem2;
      mem2 := !mem3;
      mem3 := (!mem1 + !mem2);
      x := !x + 1
    done;
    !mem3;;


