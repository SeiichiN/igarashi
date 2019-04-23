(* プログラミング in OCaml 練習問題 14.1 *)

(* 練習問題 14.1 *)

(*
 `Nil、`Cons を使って表現されるリストに対して、append、map、downto1関数を定義しなさい。（第5章参照）

# append;;
- : ([< `Cons of 'b * 'a | `Nil] as 'a) ->
   ([> `Cons of 'b * 'c] as 'c) -> 'c
= <fun>

# map;;
- : ('a -> 'b) ->
   ([< `Cons of 'a * 'c | `Nil ] as 'c) ->
   ([> `cons of 'b * 'd | `Nil ] as 'd
= <fun>

# downto1;;
- : int -> ([> `Cons of int * 'a | `Nil ] as 'a) = <fun>
*)

