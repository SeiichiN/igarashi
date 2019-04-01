(* 08-channel.ml *)

(* 8.5 チャネルを使った入出力 *)

(* 入力 *)
open_in;;

input_char;;

input_line;;

input_byte;;

close_in;;

(* 出力 *)
open_out;;

output_char;;

output_string;;

output_byte;;

close_out;;

(* 既存のファイルの最後に書き足す場合 *)
open_out_gen [Open_wronly; Open_append; Open_text] 0o666 "XXX";;
(* この場合、「XXX」は、実際に存在するファイル名 *)

(* 標準入出力、標準エラー出力関連の関数 *)
print_char;;
print_string;;
print_int;;
print_float;;
print_endline "";;
print_newline ();;

prerr_char;;
prerr_string;;
prerr_int;;
prerr_float;;
prerr_endline "";;
prerr_newline ();;

(stdin, stdout, stderr);;

print_endline "AAAAA";;
print_int 1;;
print_newline ();

