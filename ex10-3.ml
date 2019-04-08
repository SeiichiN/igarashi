(* プログラミング in OCaml 練習問題 10.3 *)

(* 練習問題 10.3 *)

(*
UNIX の fold コマンドは、ファイル名の列を引数として、その内容を、長い行を複数の短い行（デフォルトでは 80 文字）に分割しながら表示します。これを OCaml で実装し、実行可能ファイルを作成しなさい。オプションとしては、行の長さを整数で指定する --width を実装しなさい。
*)

let ver = "0.1";;

let display_linenum = ref false;; (* 行番号を表示するかどうかを示す *)

let filenames = ref [];;  (* 処理するファイル名をためておく *)

let spec = [("-n", Arg.Set display_linenum, "Display line number");
            ("--width", Arg.Set line_width, "Display line width");
            ("-version",
            Arg.Unit
            (fun () -> Printf.printf "cat in OCaml ver: %s\n" ver),
            "Display version number")];;


let display_file filename =
  let oc = open_in filename
  and line = ref 1
  and chars = ref "a"
  and end_of_file = ref false in
  while (!end_of_file == false) do
      try
          chars := input_char oc;
          if !display_linenum
          then
            Printf.printf "%4d " !line
          else ();
          print_endline !s;
          line := !line + 1
      with End_of_file -> end_of_file := true
  done;
  close_in oc


let _ =
    Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: cat [-n] [-help] [-version] filename ...";

    (* この時点で display_linenum, filenames が変更されているはず *)
    List.iter (fun s -> display_file s)
    (List.rev !filenames);;

