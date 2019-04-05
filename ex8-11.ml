(* プログラミング in OCaml 練習問題 8.11 *)

(* 練習問題 8.11 *)

(*
ファイル名を引数にとって、そのファイルの内容を行番号付きで表示させる関数 display_file を定義しなさい。
 *)

(* 予備知識
 * ファイルは、チャンネルに結び付けられる。そのチャンネルを oc とする。
let oc = open_in "ex8-11.txt";;
*)

(* チャンネルから1行読み込む
input_line oc;;
*)

(* 解答 *)
let display_file filename =
  let oc = open_in filename
  and line = ref 1
  and s = ref "a"
  and jo = ref true in
  while (!jo == true) do
      try
          s := input_line oc;
          (* print_int !line; *)
          Printf.printf "%4d" !line;
          print_string " ";
          print_endline !s;
          line := !line + 1
      with End_of_file -> jo := false
  done;
  close_in oc
;;

(* 実行例 -- Printf.printf "%4d" !line の場合
 *
# display_file "ex8-11.ml";;
   1 (* プログラミング in OCaml 練習問題 8.11 *)
   2
   3 (* 練習問題 8.11 *)
   4
   5 (*
   6 ファイル名を引数にとって、そのファイルの内容を行番号付きで表示させる関数 display_file を定義しなさい。
   7  *)
   8
   9 (* 予備知識
  10  * ファイルは、チャンネルに結び付けられる。そのチャンネルを oc とする。
  11 let oc = open_in "ex8-11.txt";;
  12 *)
  13
  14 (* チャンネルから1行読み込む
  15 input_line oc;;
*)


