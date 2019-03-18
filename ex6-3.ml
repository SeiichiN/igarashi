(* 練習問題 6.3 *)

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

(*
足し算の定義：
・ゼロに自然数 n を足したものは n である。
・m より 1 大きい数に n を足したものは、m と n を足したものより 1 大きい数である。
 *)
let rec add m n =
  match m with
    Zero -> n
  | OMT m' -> OMT (add m' n);;

(* 整数リスト *)
type intlist = INil | ICons of int * intlist;;

(* 掛け算 *)  
let rec mul m n =
  match n with
    Zero -> zero
  | OMT n' -> add (mul m n') m;;
  
(* 引き算 *)
(*
引き算の定義：
・自然数 m からゼロをひくと m である。
・n より 1 大きい数を m から引いたものは、m から n 引いた数に 1 足したものである。
 *)
let rec monus m (n: nat) =
  match m with
    Zero -> zero
  | OMT m' -> OMT (monus m' n);;
  
