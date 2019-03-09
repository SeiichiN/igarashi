(* 練習問題 5.3 *)

(*
 * リストを集合とみなして、以下の集合演算をする関数を定義しなさい。
 *)

(* 1: mem a s で、 a が s の要素かどうかを判定する関数 mem *)

let rec mem a s =
    match s with
    [] -> false
    | v :: rest ->
            if a = v
            then true
            else mem a rest;;

let test1 = mem 1 [2; 3; 5; -6; 1; 30] = true;;
let test2 = mem 4 [2; 3; 5; -6; 1; 30] = false;;

let s1 = [1; 2; -5; 20; 4];;
let s2 = [2; 3; -5; 34; 4];;

let cons a b = a :: b;;
let test99 = cons 1 [] = [1];;
(* let test98 = cons 1 2 = [1; 2];; *)

(* 2: intersect s1 s2 で s1 と s2 の共通部分を返す関数 *)

let intersect s1 s2 =
    let rec intersect_in s1 s2 common =
        match s1 with
        [] -> common
    | v1 :: rest1 ->
            match s2 with
            [] -> []
    | v2 :: rest2 ->
            if v1 = v2
            then
                intersect_in v1 rest2 (v1 :: common)
            else
    intersect_in v1 rest2 common


let test11 = intersect s1 s2 = [2; -5; 4];;


(* 3: union s1 s2 で s1 と s2 の和を返す関数 union *)


(* 4: diff s1 s2 で s1 と s2 の差を返す関数 diff *)
