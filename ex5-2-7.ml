(* 練習問題 5.2.7 *)
(*
 * リストと、リストの要素上の述語pを満たすすべての要素のリストを返す関数 filter
 *
 * # filster;;
 * - : ('a -> bool) -> a' list -> 'a list = <fun>
 *)

let rec filter fn l =
    match l with
    [] -> []
    | v :: rest ->
            if (fn v)
            then v :: filter fn rest
            else filter fn rest;;



(* 関数 length  *)
#use "ex5-2-3.ml";;

let is_positive x = (x > 0);;

let test21 = filter is_positive [-9; 0; 2; 5; -3] = [2; 5];;
let test22 = filter (fun l -> length l = 3) [[1; 2; 3]; [4; 5]; [6; 7; 8]; [9]] =
    [[1; 2; 3]; [6; 7; 8]];;

