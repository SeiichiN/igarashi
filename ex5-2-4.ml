(* 練習問題 5.2.4 *)

(*
 * 与えられたリストのリストに対し、内側のリストの要素を並べたリストを
 * 返す関数 concat を定義せよ。
 *
 * # concat;;
 * - : 'a list list -> 'a list = <fun>
 *)

let rec concat = function
    [] -> []
    | v :: rest ->
            v @ concat rest;;

let test1 = concat [[0; 3; 4]; [2]; []; [5; 0]] = [0; 3; 4; 2; 5; 0];;

