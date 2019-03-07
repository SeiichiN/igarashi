(* 練習問題 5.2.8 *)

(*
 * 先頭からn番目までの要素からなる部分リストを取り出す関数 take と
 * n番目までの要素を抜かした部分リストを取り出す関数 drop
 *)

let take n l =
    let rec take_inner n l a =
        match l with
    [] -> []
    | v :: rest ->
            if n = 0
            then a
            else
                v :: take_inner (n-1) rest :: a
    in
    take_inner n l [];;


#use "ex5-2.ml";; (* 関数 downto1 *)

let ten_to_zero = downto1 10;;

let test1 = take 8 ten_to_zero = [10; 9; 8; 7; 6; 5; 4; 3];;

let test2 = drop 7 ten_to_zero = [3; 2; 1];;

