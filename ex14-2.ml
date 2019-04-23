(* プログラミング in OCaml 練習問題 14.2 *)

(* 練習問題 14.2 *)

(*
拡張リストを単なるリストに変換する関数 list_of_alist を定義しなさい。

# list_of_alist;;
- : ([< `App of 'a * 'a | `Cons of 'b * 'a | `Nil ] as 'a) ->
    ([> `Cons of 'b * 'c | `Nil ] as 'c)
    = <fun>
*)

#use "ex14-1.ml";;

let l1 = `Nil
and l2 = `Cons (1, `Nil)
and l3 = `Cons (2, `Cons (1, `Nil));;

let l4 = `Cons (3, `Cons (4, `Nil));;

let l6 = `Cons (1, `App(l3, l4));;

let rec list_of_alist = function
    `Nil -> `Nil
    | `Cons (a, l) -> 
            `Cons (a, list_of_alist l)
    | `App (l1, l2) ->
            append (list_of_alist l1) (list_of_alist l2);;
(*
val list_of_alist :
  ([< `App of 'a * 'a | `Cons of 'b * 'a | `Nil ] as 'a) ->
  ([ `Cons of 'b * 'c | `Nil ] as 'c) = <fun>
*)

list_of_alist l6;;
(*
- : [ `Cons of int * 'a | `Nil ] as 'a =
`Cons (1, `Cons (2, `Cons (1, `Cons (3, `Cons (4, `Nil)))))
*)
