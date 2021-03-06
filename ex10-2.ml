﻿(* プログラミング in OCaml 練習問題 10.2 *)

(* 練習問題 10.1 *)

(*
UNIX の wc コマンドは、ファイル名の列を引数として、それぞれのファイルの行数（改行文字の数）、空白で区切られた単語の数、バイト数を表示するものです。これを OCaml で実装し、実行可能ファイルを作成しなさい。オプションとしては、バイト数だけ表示する -c、行数だけを表示する -l、単語数だけを表示する -w を実装しなさい。
 *)

let ver = "0.1";;

let count_lines = ref false (* 行数を表示するかどうかを示す *)
and count_bytes = ref false
and count_words = ref false;;


let filenames = ref [];;  (* 処理するファイル名をためておく *)

let spec = [("-l", Arg.Set count_lines, "Display number of lines");
            ("-c", Arg.Set count_bytes, "Display number of bytes");
            ("-w", Arg.Set count_words, "Display number of words");
            ("-version",
            Arg.Unit
            (fun () -> Printf.printf "cat in OCaml ver: %s\n" ver),
            "Display version number")];;

let rec assoc a = function
    [] -> ""
  | (a', b) :: rest -> if a = a' then b else assoc a rest;;

let display_count filename =
  let oc = open_in filename
  and lines = ref 0
  and bytes = ref 0
  and words = ref 0
  and moji = ref 'a'
  and end_of_file = ref false in
  while (!end_of_file == false) do
      try
        moji := input_char oc;
        bytes := !bytes + 1;
        if !moji = ' ' then words := !words + 1;
        if !moji = '\n' then lines := !lines + 1
      with End_of_file -> end_of_file := true
  done;
close_in oc;
[("bytes", string_of_int !bytes);
 ("words", string_of_int !words);
 ("lines", string_of_int !lines)];;

let _ =
    Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: wc [-c] [-w] [-l] [-help] [-version] filename ...";

    (* この時点で display_linenum, filenames が変更されているはず *)
    List.iter 
      (fun s -> 
         if !count_bytes 
         then Printf.printf "C:%s   " (assoc "bytes" (display_count s));
         if !count_words 
         then Printf.printf "W:%s   " (assoc "words" (display_count s));
         if !count_lines 
         then Printf.printf "L:%s   " (assoc "lines" (display_count s));
         Printf.printf "filename: %s\n" s)
      (List.rev !filenames);;

