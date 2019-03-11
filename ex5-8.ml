(* 練習問題 5.8 *)

(*
 * map の定義は末尾再帰的でないため、リストの長さが長くなるとそれに比例した量のメモリ（スタック）が必要になります。map と同機能で、必要なメモリ量が一定である map2 を定義しなさい。（ヒント：ある末尾再帰的な関数を補助的に使います）。
 *)

(* 整数リストを2倍する *)
let rec map f = function
    [] -> []
  | v :: rest -> f v :: map f rest;;
  
let test1 = map (fun x -> x * 2) [4; 91; 0; -34] = [8; 182; 0; -68];;

let rec fold_right f l e =
    match l with
    [] -> e
  | v :: rest -> fold_right f rest ((f v) :: e);;

let twice x = x * 2;;

let mylist = [1; 2; 3];;

let test2 = fold_right twice mylist [] = [2; 4; 6];;

