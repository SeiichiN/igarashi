(* 練習問題 5.2.3 *)

(* 与えられたリストのリストに対し、（内側のリストの）要素の総数を返す関数 nested_length
を定義せよ。 *)

(*
# nested_length;;
- : 'a list list -> int = <fun>

# nested_length [[1; 2; 3]; [4; 5]; [6]; [7; 8; 9; 10]];;
- : int = 10
 *)

let length l = 
    let rec nagasa l a = 
        match l with
        [] -> a
        | v :: rest -> 
                nagasa rest (a+1) 
    in
    nagasa l 0;;

let test1 = length [1; 2; 3] = 3;;

        
