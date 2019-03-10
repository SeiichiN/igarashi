(* 練習問題 5.5 *)

(*
 * concat, forall, exists を fold_right を使って定義しなさい。
 *)

(* concat -- 与えられたリストのリストに対し、内側のリストの要素を並べたリストを返す *)
(* concat [[0; 3; 4]; [2]; []; [5; 0]];; *)
(* - : int list = [0; 3; 4; 2; 5; 0];; *)

(* forall -- 与えられたリスト要素すべてが、ある与えられた性質を満たすかどうかを検査する *)

(* exists -- リストの中に性質を満たす要素があるかどうかを検査する  *)

(* fold_right -- リストの全要素を使って順に計算する
 *            -- fold f l e
 *                 f -- 関数
 *                 l -- リスト
 *                 e -- タネ
 *)

let concat l =
    let rec fold_right f l e =
        match l with
        [] -> e
        | v :: rest ->
                fold_right f rest (e @ (f v))
    in
    fold_right (fun x -> x) l [];;

let test_concat = concat [[0; 3; 4]; [2]; []; [5; 0]] = [0; 3; 4; 2; 5; 0];;

let forall f l =
    let rec fold_right f l =
        match l with
        [] -> true
        | v :: rest ->
                if (f v)
                then fold_right f rest
                else false
    in
    fold_right f l;;

let test_forall1 = forall (fun x -> x > 0) [1; 2; 3; 4] = true;;
let test_forall2 = forall (fun x -> x > 0) [1; 2; 3; -4] = false;;


let exists f l =
    let rec fold_right f l =
        match l with
        [] -> false
        | v :: rest ->
                if (f v)
                then true
                else fold_right f rest
    in
    fold_right f l;;

let test_exists1 = exists (fun x -> x > 10) [1; 4; 11; 3] = true;;
let test_exists2 = exists (fun x -> x > 10) [1; 4; 10; 3] = false;;

