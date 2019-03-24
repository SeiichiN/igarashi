(* 練習問題 7.1 *)

(*
例外処理機構を使って、オプション型を返り値とする、find 関数を書きなさい。

# find;;
- : 'a -> 'a list -> int option = <fun>

# find 7 [0; 8; 7; 3];;
- : int option = Some 3

#find 9 [0; 8; 7; 3];;
- : int option = None
*)

let find x =
    let rec find_in x e = function
        [] -> None
    | v :: rest when v = x -> Some e
    | _ :: rest ->
            find_in x (e+1) rest
    in
    find_in x 1
;;

let find x =
    let rec find_in x e = function
        [] -> None
    | v :: rest ->
            if v = x then Some e
            else
                find_in x (e+1) rest
    in
    find_in x 1
;;

