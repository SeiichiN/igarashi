(* 練習問題 5.6 *)

(*
 * quick_sort関数を @ を使わないように書き換えなさい。（ヒント：partition を改造したときのように、、引数をひとつ増やして、整列済みのリストを受け取るようにします）。
 *)

let rec partition pivot = function
    [] -> ([], [])
    | y :: rest ->
            let (left, right) = partition pivot rest in
            if pivot < y
            then (left, y::right)
            else (y::left, right);;

let test1 = partition 7.0 [9.; 1.; 5.; 4.; 18.] = ([1.; 5.; 4.], [9.; 18.]);;

let rec quick_sort = function
    [] -> []
    | pivot :: rest ->
            let (left, right) = partition pivot rest in
            quick_sort left @ (pivot :: quick_sort right);;

let test2 = quick_sort [9.0; 1.0; 5.0; 4.0; 18.0] =
    [1.0; 4.0; 5.0; 9.0; 18.0];;
