(* 練習問題 5.4 *)
(*
 * f, g を適当な型の関数とします。map f (map g l) を map を一度しか使用しない同じ意味の式に書き換えなさい。map (fun x -> ... ) l の ... の部分はどうなるでしょう？
 *)


let rec map g l =
    match l with
    [] -> []
    | v :: rest ->
            (g v) :: map g rest;;

let test1 = map (fun x -> x * 2) [1; 2; 3] = [2; 4; 6];;

map (fun x -> x * x) (map (fun x -> x * 2) [1; 2; 3]);;

(*
let rec map (f g) l =
    match l with 
    [] -> []
    | v :: rest ->
            (f g) v :: (map (f g) rest);;
*)

map (fun x -> (x * 2) * (x * 2)) [1; 2; 3];;

let f x = x * x;;
let g x = x * 2;;

let test11 = f 3 = 9;;
let test12 = g 3 = 6;;

map (fun x -> f (g x) ) [1; 2; 3];;


