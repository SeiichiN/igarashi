(* 練習問題 5.2.9 *)

(*
 * （空でない）リストの中から最大値を返す関数 max_list
 *)

let max_list l =
    let rec max_list_in l max =
        match l with
        [] -> max
        | v :: rest ->
                if v <= max
                then max_list_in rest max
                else max_list_in rest v
    in
    max_list_in l 0;;

let test1 = max_list [7; 9; 0; -5] = 9

