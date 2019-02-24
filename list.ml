(* 再帰的多相的データ構造：リスト *)

[3; 9; 0; -10];;

let week = ["Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"];;

let oddnums = [3; 9; 253];;
let more_oddnums = 99 :: oddnums;;
let evennums = 4 :: 10 :: [256; 12];;
let boollist = true :: false :: [];;
let campuslist = "Yoshida" :: "Uji" :: "Katsura" :: [];;

(::)(1, [2]);;
(::)(3, []);;

let cons = fun (x, y) -> (::)(x, y);;

let p = (1, [2]) in cons p;;

[(fun x -> x + 1); (fun x -> x * 2)];;

[1; 2; 3] :: [[4; 5]; []; [6; 7; 8]];;

let sum_list3 [x; y; z] = x + y + z;;

(* sum_list: 整数リストの全要素の和を計算する関数 *)
let rec sum_list l =
    match l with
    [] -> 0
    | n :: rest -> n + (sum_list rest);;

let rec sum_list = function
    [] -> 0
    | n :: rest -> n + (sum_list rest);;

