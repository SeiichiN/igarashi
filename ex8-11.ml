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
          print_int !line;
          print_string " ";
          print_endline !s;
          line := !line + 1
      with End_of_file -> jo := false
  done;
  close_in oc
;;

