(* プログラミング in OCaml 練習問題 8.6 *)

(* 練習問題 8.6 *)

(*
本文中の関数 whle を参考にして、for 式相当の機能を実現する再帰関数を定義しなさい。
 *)

(* while を再帰関数であらわす *)
let rec whle condition body =
    if condition () then
        begin body (); whle condition body end;;
(* 条件部・本体部を関数とすることで、適用のたびに違った値が返ってくる可能性が
 * 生まれる。
 *)

(* fact を whle を使って記述 *)
let fact n =
    let i = ref 1 and res = ref 1 in
    whle (fun () -> (!i <= n))
    (fun () -> res := !res * !i; i := !i + 1);
    !res;;


  (* for を再帰関数で表す *)
let rec foor condition body =
  if condition () then
    begin body (); foor condition body end;;

  (* fact を foor を使って記述 *)
let fact n =
  let i = ref 1 and res = ref 1 in
  foor (fun () -> (!i <= n))
       (fun () -> res := !res * !i; i := !i + 1);
  !res;;
  
  
