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

  (* リストの長さを返す関数 *)
let rec length = function
    [] -> 0
  | _ :: rest -> 1 + length rest;;

let test11 = length [1; 2; 3] = 3;;

  (* ふたつのリストを連結する関数 *)
let rec append l1 l2 =
  match l1 with
    [] -> l2
  | v :: l1' -> v :: (append l1' l2);;

let test12 = append [1; 2; 3] [4; 5; 6] = [1; 2; 3; 4; 5; 6];;
let test13 = append [5; 6] [] = [5; 6];;
let test14 = append [] [true; false] = [true; false];;

let test15 = [1; 2; 3] @ [4; 5; 6] = [1; 2; 3; 4; 5; 6];;

  (* リストを反転させる reverse関数 *)
let rec reverse = function
    [] -> []
  | v :: rest -> reverse rest @ [v];;

let test21 = reverse [1; 2; 3] = [3; 2; 1];;
let test22 = reverse [] = [];;
  
let rec revAppend l1 l2 =
  match l1 with
    [] -> l2
  | v :: l1' -> revAppend l1' (v :: l2)
let rev l = revAppend l [];;

let test23 = revAppend [1; 2; 3] [4; 5; 6] = [3; 2; 1; 4; 5; 6];;
let test24 = rev [1; 2; 3] = [3; 2; 1];;

(* 整数リストを2倍する *)
let rec map f = function
    [] -> []
  | v :: rest -> f v :: map f rest;;
  
let test31 = map (fun x -> x * 2) [4; 91; 0; -34] = [8; 182; 0; -68];;
  
  (* 与えられたリストの要素すべてが、ある与えられた性質を満たすかどうか *)
let rec forall p = function
    [] -> true
  | v :: rest ->
     if p v
     then forall p rest
     else false;;

let test32 = forall (fun x -> x >= 5) [9; 29; 5] = true;;
let test33 = forall (fun x -> x >= 5) [6; 3; 9] = false;;

  (* 与えられたリストの中に、ある与えられた性質を満たす要素があるかどうか *)
let rec exists p = function
    [] -> false
  | v :: rest ->
     if p v
     then true
     else exists p rest;;
             
let test34 = exists (fun x -> (x mod 7) = 0) [23; -98; 19; 53] = true;;

let rec fold_right f l e =
    match l with
    [] -> []
  | v :: rest -> f v e :: fold_right f rest e;;

let test41 = fold_right (fun x y -> x + y) [3; 5; 7] 1 = [4; 6; 8];;

let rec fold_left f l e =
    match l with
    [] -> []
  | v :: rest -> fold_left f rest e :: f v e;;

let test42 = fold_left (fun x -> x - y) [5; 7; 9] 1 = [4; 6; 8];;

