(* 練習問題 7.2 *)

(*
整数リストの要素すべての積を返す関数 prod_list を定義しなさい。リスト要素にひとつでも 0 が含まれている場合には、prod_list の適用結果は常に 0 になるので、例外処理を利用して、0 を発見したら残りの計算を行わずに中断して 0 を返すように定義しなさい。
 *)

let prod_list l =
  let rec prod_list_in l e =
    match l with
      [] -> e
    | v :: rest ->
       if v = 0 then 0
       else prod_list_in rest (v * e)
  in
  prod_list_in l 1;;

exception Found_zero;;
  
let prod_list l =
  let rec prod_list_in l e =
    match l with
      [] -> e
    | v :: rest ->
       if v = 0 then raise Found_zero
       else prod_list_in rest (v * e)
  in
  try prod_list_in l 1 with Found_zero -> 0;;

let test1 = prod_list [2; 3; 4] = 24;;
let test2 = prod_list [2; -3; 4] = -24;;
let test3 = prod_list [2; 0; 4] = 0;;

  
