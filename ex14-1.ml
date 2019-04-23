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

let l1 = `Cons (1, `Cons (2, `Nil));;
let l2 = `Cons (3, `Cons (4, `Nil));;

let rec append l1 l2 =
    match l1 with
    `Nil -> l2
    | `Cons (a,  rest) ->
          `Cons(a, append rest l2);;
(* val append :
  ([< `Cons of 'b * 'a | `Nil ] as 'a) -> ([> `Cons of 'b * 'c ] as 'c) -> 'c =
  <fun>  *)

append l1 l2;;
(* - : [> `Cons of int * 'a | `Nil ] as 'a =
`Cons (1, `Cons (2, `Cons (3, `Cons (4, `Nil))))  *)

let rec map f = function
    `Nil -> `Nil
    | `Cons (a, l) ->
            `Cons((f a), map f l);;
(* val map :
  ('a -> 'b) ->
  ([< `Cons of 'a * 'c | `Nil ] as 'c) -> ([> `Cons of 'b * 'd | `Nil ] as 'd) =
  <fun>  *)

map (fun x -> x * 2) l1;;
(*  - : [> `Cons of int * 'a | `Nil ] as 'a = `Cons (2, `Cons (4, `Nil))  *)

let rec downto1 n =
    if n = 1 then `Cons (1, `Nil)
    else
        `Cons (n, downto1 (n-1));;
(*  val downto1 : int -> ([> `Cons of int * 'a | `Nil ] as 'a) = <fun>  *)

downto1 10;;
(*
- : [> `Cons of int * 'a | `Nil ] as 'a =
`Cons
  (10,
   `Cons
     (9,
      `Cons
        (8,
         `Cons
           (7,
            `Cons
              (6, `Cons (5, `Cons (4, `Cons (3, `Cons (2, `Cons (1, `Nil))))))))))
*)

