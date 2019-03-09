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

(* ========================================================= *)
(* 2: intersect s1 s2 で s1 と s2 の共通部分を返す関数 *)

let test97 = mem 3 [1; 2; 3; 4; 5; 6] = true;;
let test96 = mem 7 [1; 2; 3; 4; 5; 6] = false;;

(* 補助関数: リスト l の末尾に a を加える関数 *)
let rec add_last a l =
    match l with
    [] -> a :: []
    | v :: rest ->
            v :: (add_last a rest);;

let test95 = add_last 3 [2; 4; 6] = [2; 4; 6; 3];;

(* 2つのリスト s1 と s2 の共通部分を返す関数 *)
let intersect s1 s2 =
    let rec intersect_in s1 s2 common =
        match s1 with
        [] -> common
    | v :: rest ->
            if (mem v s2)
            then intersect_in rest s2 (add_last v common)
            else intersect_in rest s2 common
    in
    intersect_in s1 s2 [];;

let test11 = intersect s1 s2 = [2; -5; 4];;

(* ================================================================ *)
(* 3: union s1 s2 で s1 と s2 の和を返す関数 union *)

let union s1 s2 =
    let rec union_in s1 s2 ac =
        match s1 with
        [] -> ac @ s2
    | v :: rest ->
            if (mem v s2)
            then union_in rest s2 ac
            else union_in rest s2 (add_last v ac)
    in
    union_in s1 s2 [];;

let test21 = union s1 s2 = [1; 20; 2; 3; -5; 34; 4];;

(* ================================================================ *)
(* 4: diff s1 s2 で s1 と s2 の差を返す関数 diff *)

(* 和集合から共通集合を引くことで、差の集合がでる *)
let diff s1 s2 =
    let inter_list = intersect s1 s2 in (* 共通部分 *)
    let union_list = union s1 s2 in (* 和 *)
    let rec diff_in un_list in_list ac =
        match un_list with
        [] -> ac
    | v :: rest ->
            if (mem v in_list)
            then diff_in rest in_list ac
            else diff_in rest in_list (add_last v ac)
    in
    diff_in union_list inter_list [];;

let test31 = diff s1 s2 = [1; 20; 3; 34];;

