(* 第6章 レコードとヴァリアント *)

type student = {name : string; id : int};;

let st1 = {name = "Taro Yamada"; id = 123456 }
and st2 = {id = 51; name = "Ichiro Suzuki"};;

let st3 = { st1 with id = 234567 };;

type foo = {name : bool; };;

{name = "Ichiro Suzuki"; id = 51};;

let string_of_student {name = n; id = i} =
    n ^ "'s ID is " ^ string_of_int i;;

string_of_student st1;;

