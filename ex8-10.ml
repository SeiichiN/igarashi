(* プログラミング in OCaml 練習問題 8.10 *)

(* 練習問題 8.10 *)

(*
print_int 関数を stdout, output_string などを用いて定義しなさい。
 *)

output_string stdout "aaa";;
(* aaa- : unit = ()  *)

print_int 32;;
(* 32- : unit = ()  *)

let print_inte n =
    let s = ref "a" in
    s := string_of_int n;
    output_string stdout !s;;

(* 実行例 *)
print_inte 33;;
(* 33- : unit = ()  *)

