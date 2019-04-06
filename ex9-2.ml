(* プログラミング in OCaml 練習問題 9.2 *)

(* 練習問題 9.2 *)

(*
二分探索木を使ったテーブルを、シグネチャとして TABLE2 を与えたモジュールとして実装しなさい。そして、各関数が機能していることを確かめなさい。
 *)

module Tree =
    struct
        type 'a t = Lf | Br of 'a * 'a t * 'a t

        let rec size = function
            Lf -> 0
            | Br (_, left, right) -> 1 + size left + size right

        let rec depth = function
            Lf -> 0
            | Br (_, left, right) -> 1 + max (depth left) (depth right)
    end;;

let tr = Tree.Br (1, Tree.Lf, Tree.Br(2, Tree.Lf, Tree.Lf));;

Tree.depth tr;;

