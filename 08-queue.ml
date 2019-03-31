(* 08-queue.ml *)
(* 8.4 Case Study : 参照を使ったデータ構造 - キュー *)

type 'a mlist = MNil | MCons of 'a * 'a mlist ref;;

type 'a queue = {mutable head : 'a mlist; mutable tail : 'a mlist};;

(* create -- 新しいキューをつくる = 空のキュー *)
let create () = {head = MNil; tail = MNil};;

(* 実行例 *)
let q : int queue = create ();;

(* add -- 要素を加えるための関数 *)
let add a = function
    {head = MNil; tail = MNil} as q ->
        let c = MCons (a, ref MNil) in
        q.head <- c;
        q.tail <- c
    | {tail = MCons(_, next)} as q ->
            let c = MCons (a, ref MNil) in
            next := c;
            q.tail <- c
    | _ -> failwith "enqueue: input queue broken";;

add 1 q; add 2 q; add 3 q;;
q;;

