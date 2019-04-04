(* プログラミング in OCaml 練習問題 8.11 *)

(* 練習問題 8.11 *)

(*
ファイル名を引数にとって、そのファイルの内容を行番号付きで表示させる関数 display_file を定義しなさい。
 *)

let oc = open_in "ex8-11.txt";;
(* Exception: Sys_error "ex8-11.txt: No such file or directory". *)


input_line oc;;

let display_file filename =
  let oc = open_in filename and line = ref 1 and s = ref "" in
  while (!line < 10) do
    s := input_line oc;
    print_newline !s;
    line := !line + 1
  done;;

