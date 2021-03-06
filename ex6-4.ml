

(* 練習問題 6.4 *)

(*
nat 上の掛け算を行う関数 mul と nat 上の引き算を行う関数（ただし負になる場合は 0 を返す）monus （モーナス）を定義しなさい。（ただし、整数などに変換して *、- などを使わず nat 型の値から「直接」計算するように定義しなさい）。
 *)

type nat = Zero | OMT of nat;;  (* OMT -- OneMoreThan *)

let zero = Zero
and one = OMT Zero
and two = OMT (OMT Zero);;

let three = OMT two;;
let four = OMT three;;
let five = OMT four;;
let six = OMT five;;

  
(* 引き算 *)
(*
ここを見た：
http://gifnksm.hatenablog.jp/category/OCaml?page=1205602283
 *)
let rec monus m n =
  match (m, n) with
    (Zero, _) -> zero
  | (_, Zero) -> m
  | (OMT m', OMT n') -> monus m' n';;
  
(*
 * 上の monus 関数を改造して、引き算の答えが負になるような場合には None を返す nat -> nat -> nat option 型の関数 minus を定義しなさい。
 *)

let rec minus m n =
  match (m, n) with
    (Zero, _) -> None 
  | (_, Zero) -> Some m
  | (OMT m', OMT n') -> minus m' n';;

