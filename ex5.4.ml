(* 練習問題 5.4 *)
(*
 * f, g を適当な型の関数とします。map f (map g l) を map を一度しか使用しない同じ意味の式に書き換えなさい。map (fun x -> ... ) l の ... の部分はどうなるでしょう？
 *)

(*
let map f (map g l)
*)

let map (fun x ->
    (fun l ->
        match l with
        [] -> []
        | v :: rest ->

