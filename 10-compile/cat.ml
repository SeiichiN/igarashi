let ver = "0.1";;

let display_linenum = ref false;; (* 行番号を表示するかどうかを示す *)

let filenames = ref [];;  (* 処理するファイル名をためておく *)

let spec = [("-n", Arg.Set display_linenum, "Display line number");
            ("-version",
            Arg.Unit
            (fun () -> Printf.printf "cat in OCaml ver: %s\n" ver),
            "Display version number")];;

let _ =
    Arg.parse spec
    (fun s -> filenames := s :: !filenames)
    "Usage: cat [-n] [-help] [-version] filename ...";

    (* この時点で display_linenum, filenames が変更されているはず *)
    if !display_linenum then print_endline "-n was turned on";
    List.iter (fun s -> Printf.printf "filename specified is: %s\n" s)
    (List.rev !filenames);;


