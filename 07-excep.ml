(* 7: 例外処理 *)

let rec nth x = function
    [] -> None
    | v :: rest ->
            if x = 1 then Some v
            else
                nth (x-1) rest
;;

let rec find x = function
    [] -> 0
    | a :: l when a = x -> 1
    | _ :: l -> 1 + find x l;;


let test1 = find 7 [0; 8; 7; 3] = 3;;
let test2 = find 9 [] = 0;;
let test3 = find 9 [0; 8; 7; 3] = 4;; (* <== まちがい *)

let rec nth n l =
    match (n, l) with
    (n, _) when n <= 0 -> None
    | (1, a::_) -> Some a
    | (_, _::rest) -> nth (n-1) rest
    | (_, []) -> None
;;

let test_nth1 = nth 3 [1; 0; 8; 7] = Some 8;;
let test_nth2 = nth 9 [1; 0; 8; 7] = None;;
let test_nth3 = nth 0 [1; 0; 8; 7] = None;;


let rec find x = function
    [] -> None
    | a :: l when a = x -> Some 1
    | _ :: l -> 
            match find x l with
            None -> None
    | Some i -> Some (i+1)
;;

let test1 = find 7 [0; 8; 7; 3] = Some 3;;
let test2 = find 9 [] = None;;
let test3 = find 9 [0; 8; 7; 3] = None;; 

(* 実数のリストの各要素の平方根をとる *)
let rec map f = function
    [] -> []
    | v :: rest ->
            (f v) :: (map f rest)
;;

let map_sqrt l = map sqrt l;;

(* 要素に負数が含まれている場合、None を返すようにする *)
let my_sqrt x =
    if x < 0.0
    then None
    else Some (sqrt x);;

let map_sqrt l = map my_sqrt l;;

let test_map1 = map_sqrt [2.5; -3.6; 4.9] =
    [Some 1.58113883008418976; None; Some 2.21359436211786553] ;;

(* 返り値がオプション型のりすとになってしまうので *)
let rec map_sqrt = function
    [] -> Some []
    | x :: rest ->
            if x < 0.0 then None
            else
                match map_sqrt rest with
                None -> None
    | Some l -> Some (sqrt x :: l)
;;

let test_map2 = map_sqrt [2.5; -3.6; 4.9] = None;;
let test_map3 = map_sqrt [1.0; 25.0; 0.49] = Some [1.; 5.; 0.7];;

(* 例外の発生 raise *)
let f x =
    if raise Division_by_zero
    then raise Division_by_zero
    else x;;

let rec fact n =
    if n < 0
    then raise (Invalid_argument "fact: negative argumant")
    else
        if n = 0 then 1
        else n * fact (n-1)
;;

let test_raise1 = fact 4 = 24;;
(* let test_raise2 = fact (-4);;  *)
    (* Invalid_argument "fact: negative argumant" *)

let rec find x = function
    [] -> raise Not_found
    | a :: l when a = x -> 1
    | _ :: l -> 1 + find x l;;

(*
utop # find 7 [0; 8; 7; 3];;
- : int = 3
utop # find 9 [0; 8; 7; 3];;
Exception: Not_found.
*)


let rec find' x = function
    [] -> raise Not_found
    | a :: l when a = x -> 1
    | _ :: l -> 1 + find x l;;

let find x l = try find' x l with Not_found -> 0;;

let test_find1 = find 7 [0; 8; 7; 3] = 3;;
let test_find2 = find 9 [] = 0;;
let test_find3 = find 9 [0; 8; 7; 3] = 0;;

let map_sqrt l =
    let sqrt' x =
        if x < 0.0
        then raise (Invalid_argument "sqrt'")
        else sqrt x
    in
    try Some (map sqrt' l) with
    Invalid_argument "sqrt'" -> None;;

let test_map1 = map_sqrt [1.0; 16.0; 0.25] = Some [1.; 4.; 0.5];;
let test_map2 = map_sqrt [1.0; -16.0; 0.25] = None;;

