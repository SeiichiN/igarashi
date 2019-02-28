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

let rec fold_right f l e =
    match l with
    [] -> e
  | v :: rest -> f v (fold_right f rest e);;

let test42 = fold_right (fun x y -> x + y) [3; 5; 7] 0 = 15;;



  
let rec fold_left f e l =
    match l with
    [] -> e
  | v :: rest -> fold_left f (f e v) rest;;

let test43 = fold_left (fun x y -> y :: x) [] [1; 2; 3] = [3; 2; 1];;
  
let length l = fold_right (fun x y -> 1 + y) l 0;;

let test43 = length [1; 2; 3; 4] = 4;;

let rec nth n l =
    if n = 1 then
        let a :: rest = l in a
    else
        let a :: rest = l  in nth (n-1) rest;;


let test51 = nth 4 [1; 2; 3; 4; 5; 6] = 4;;
let test52 = nth 1 [1; 2; 3; 4; 5; 6] = 1;;
let test53 = nth 3 [2; 3; 4; 5; 6] = 4;;
(* let test54 = nth 1 [] = 0;;  <== Exception *)

let rec nth n l =
    match n with
    1 -> let a :: _ = l in a
  | _ -> let _ :: rest = l in nth (n-1) rest;;

let test54 = nth 4 [1; 2; 3; 4; 5; 6] = 4;;
let test55 = nth 1 [1; 2; 3; 4; 5; 6] = 1;;
let test56 = nth 3 [2; 3; 4; 5; 6] = 4;;


let rec nth n l =
    match (n, l) with
    (1, a :: _) -> a
  | (_, _ :: rest) -> nth (n-1) rest;;

let test57 = nth 4 [1; 2; 3; 4; 5; 6] = 4;;
let test58 = nth 1 [1; 2; 3; 4; 5; 6] = 1;;
let test59 = nth 3 [2; 3; 4; 5; 6] = 4;;

let rec nth n l =
    match (n, l) with
    (1, a :: _) -> a
  | (n', _ :: rest) when  n' > 0 -> nth (n-1) rest;;

let test60 = nth 4 [1; 2; 3; 4; 5; 6] = 4;;
let test61 = nth 1 [1; 2; 3; 4; 5; 6] = 1;;
let test62 = nth 3 [2; 3; 4; 5; 6] = 4;;

let f = function
    [] -> 0
  | x :: rest when x > 0 -> 1
  | x :: rest when x <= 0 -> 2;;

let test63 = f [] = 0;;
let test64 = f [1; 2; 3] = 1;;
let test65 = f [-34; 2; 4] = 2;;

let f = function
    [] -> 0
  | x :: rest when x > 0 -> 1
  | x :: rest (* when x <= 0 *) -> 2;;

let test66 = f [] = 0;;
let test67 = f [1; 2; 3] = 1;;
let test68 = f [-34; 2; 4] = 2;;

let f = function
    [] -> 0
  | x :: rest -> if x > 0 then 1 else 2;;


let test69 = f [] = 0;;
let test70 = f [1; 2; 3] = 1;;
let test71 = f [-34; 2; 4] = 2;;
  
let rec nth n l =
  match n with
    1 -> ( match l with a :: _ -> a)
  | n' when n' > 0 -> match l with _ :: rest -> nth (n-1) rest;;

  (******** 連想リストと assoc関数 ****************)
  "================== 連想リストと assoc関数 =====================";;
    
let city_phone = [("Kyoto", "075"); ("Osaka", "06"); ("Tokyo", "03")];;

let rec assoc a = function
    [] -> "Nothig"
   |(a', b) :: rest -> if a = a' then b else assoc a rest;;

let test81 = assoc "Osaka" city_phone = "06";;

  "=================== 整列とアルゴリズム =========================";;
  (*  *)
    
let nextrand seed =
    let a = 16807.0
    and m = 2147483647.0 in
    let t = a *. seed in
    t -. m *. floor (t /. m)
let rec randlist n seed tail =
    if n = 0 then (seed, tail)
    else
        randlist (n - 1) (nextrand seed) (seed::tail);;
  (*
tailには初期値として[]が与えられる。
(seed::tail) で、seedのリストが作成される。
最初は、(seed1::[]) で、[seed1] となる。
次にそれが tail となって、(seed2::seed1) で、[seed2; seed1]となる。
そして、それが、tail となる。
   *)
    
let rec insert x = function
    [] -> [x]
   | y :: rest when x < y -> x :: (y :: rest)
   | y :: rest -> y :: (insert x rest);;

let test91 = insert 18.5 [-2.2; 9.1; 31.8] = [-2.2; 9.1; 18.5; 31.8];;
let test92 = insert 0.7 [] = [0.7];;


let rec insertion_sort = function
    [] -> []
   | x :: rest -> insert x (insertion_sort rest);;
    
"============ Quick Sort ==================";;

let rec partition pivot = function
    [] -> ([], [])
   | y :: rest ->
           let (left, right) = partition pivot rest in
           if pivot < y 
           (* pivot が 最初の要素 y よりも小さい場合 *)
           then (left, y::right)
           else (y::left, right);;

let test93 = partition 7.0 [9.0; 1.0; 5.0; 4.0; 18.0] =
    ([1.; 5.; 4.], [9.; 18.]);;

let rec quick_sort = function
    [] -> []
   | pivot :: rest -> 
           let (left, right) = partition pivot rest in
           quick_sort left @ (pivot :: quick_sort right);;

let part pivot = function
    [] -> ([], [])
  | y :: rest ->
     let (left, right) = rest in
     if pivot < y
     then (left, y::right)
     else (y::left, right);;
  
  
