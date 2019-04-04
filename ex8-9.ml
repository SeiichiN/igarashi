(* プログラミング in OCaml 練習問題 8.9 *)

(* 練習問題 8.9 *)

(*
本文中で示したデキュー関数 dequeue の定義を正しく動作するように修正しなさい。
 *)
type 'a mlist = MNil | MCons of 'a * 'a mlist ref;;

type 'a queue = {mutable head : 'a mlist; mutable tail : 'a mlist};;

(* create -- 新しいキューをつくる = 空のキュー *)
let create () = {head = MNil; tail = MNil};;

(* 実行例 *)
let q : int queue = create ();;
q;;

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

add 1 q;;
q;;

(* peek -- 先頭要素を返す関数 *)
let peek = function
    {head = MNil; tail = MNil} -> failwith "ht: queue is empty"
  |{head = MCons(a, _)} -> a
  | _ -> failwith "hd: queue is broken";;

(* dequeue -- 先頭要素を削除して、その先頭要素を返す関数 *)
let dequeue = function
    {head = MNil; tail = MNil} ->failwith "dequeue: queue is empty"
  | {head = MCons(a, next)} as q ->
     if !next == MNil
     then (q.head <- !next; q.tail <- !next)
     else (q.head <- !next);
     a
  | _ -> failwith "dequeue: queue is broken";;
