(* プログラミング in OCaml 練習問題 10.1 *)

(* 練習問題 10.1 *)

(*
UNIX の cat コマンドは、ファイル名の列を引数として、その内容を順次表示する（標準出力に書き込む）ものです。これを OCaml で実装し、実行可能ファイルを作成しなさい。オプションに関しては少なくとも行番号を表示するための -n オプションを実装すること。
*)

let ver = "0.1";;

let display_linenum = ref false;; (* 行番号を表示するかどうかを示す *)

let filenames = ref [];;  (* 処理するファイル名をためておく *)

let spec = [("-n", Arg.Set display_linenum, "Display line number");
            ("-version",
            Arg.Unit
            (fun () -> Printf.printf "cat in OCaml ver: %s\n" ver),
            "Display version number")];;


let display_file filename =
  let oc = open_in filename
  and line = ref 1
  and s = ref "a"
  and end_of_file = ref true in
  while (!end_of_file == true) do
      try
          s := input_line oc;
          if !display_linenum
          then
            Printf.printf "%4d " !line
          else ();
          print_endline !s;
          line := !line + 1
      with End_of_file -> end_of_file := false
  done;
  close_in oc


let _ =
    Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: cat [-n] [-help] [-version] filename ...";

    (* この時点で display_linenum, filenames が変更されているはず *)
    List.iter (fun s -> display_file s)
    (List.rev !filenames);;

