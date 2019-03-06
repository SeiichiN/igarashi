(* 練習問題 5.2.5 *)

(*
 * 2つのリスト [a1; ...; an] と [b1; ...; bn] を引数として、
 * [(a1, b1); ...; (an, bn)] を返す関数 zip を定義せよ。
 * （与えられたリストの長さが異なる場合は、長いリストの
 * 余った部分を捨てて良い）
 *
 * # zip;;
 * - : 'a list -> 'b list -> ('a * 'b) list = <fun>
 *)

let rec zip list1 list2 =
    match list1 with
    [] -> []
    | v1 :: rest1 ->
            match list2 with
            [] -> []
    | v2 :: rest2 ->
            (v1, v2) :: zip rest1 rest2;;


let test1 = zip [2; 3; 4; 5; 6; 7; 8; 9; 10; 11]
[true; true; false; true; false; true; false; false; false; true] =
    [(2, true); (3, true); (4, false); (5, true); (6, false); (7, true);
    (8, false); (9, false); (10, false); (11, true)];;

let test2 = zip [1; 2; 3; 4; 5] ['a'; 'b'; 'c'; 'd'] =
    [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')];;

let test3 = zip [1; 2; 3; 4] ['a'; 'b'; 'c'; 'd'; 'e'] =
    [(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')];;


(* 練習問題 5.2.6 *)

(*
 * ペアのリスト [(a1, b1); ...; (an, bn)] を引数として、
 * リストのペア ([a1; ...; an], [b1; ...; bn]) を返す
 * 関数 unzip
 *
 * # unzip;;
 * - : ('a * 'b) list -> 'a list * 'b list = <fun>
 *)

let rec unzip = function
    [] -> ([], [])
    | (a, b) :: rest -> 
            match rest with
            [] -> ([], [])
    | (x, y) :: rest2 ->
            (a :: x, b :: y) 
             unzip rest;; 

let test11 = unzip ([(1, 'a'); (2, 'b'); (3, 'c'); (4, 'd')]) =
    ([1; 2; 3; 4], ['a'; 'b'; 'c'; 'd']);;


let test12 = unzip (zip [2; 3; 4; 5; 6; 7; 8; 9; 10; 11]
[true; true; false; true; false; true; false; false; false; true]) =
    ([2; 3; 4; 5; 6; 7; 8; 9; 10; 11],
    [true; true; false; true; false; true; false; false; false; true])
