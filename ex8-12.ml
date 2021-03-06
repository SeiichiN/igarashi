(* プログラミング in OCaml 練習問題 8.12 *)

(* 練習問題 8.12 *)

(*
二つのファイル名を引数にとって、片方のファイルの内容をもう片方にコピーする関数 cp を定義しなさい（一度開いたファイルは最後に閉じることを忘れないように）。
 *)

let cp src_file dest_file =
  let oc_src = open_in src_file
  and oc_dest = open_out dest_file
  and str = ref "a"
  and end_file = ref false in
  while (!end_file == false) do
      try
          str := input_line oc_src;
          output_string oc_dest !str;
          output_string oc_dest "\n";
      with End_of_file -> end_file := true
  done;
  flush oc_dest;
  close_out oc_dest;
  close_in oc_src
;;

(* 実行例 *)
cp "ex8-12.ml" "rensyu8-12.txt";;
