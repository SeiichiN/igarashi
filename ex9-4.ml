(* プログラミング in OCaml 練習問題 9.4 *)

(* 練習問題 9.4 *)

(*
次の抽象データ型を表す、ふたつのシグネチャをもつモジュールは、どちらもあまり実用上意味がありません。なぜでしょうか？（ヒント：empty や add のない TABLE モジュールを考えてみなさい）

module type BOGUS1 =
    sig
        type t
        
        val f: t -> int -> t
    end

module type BOGUS2 =
    sig
        type t

        val e: t
    end
 *)

module type BOGUS1 =
    sig
        type t
        
        val f: t -> int -> t
    end;;

module type BOGUS2 =
    sig
        type t

        val e: t
    end;;

module Bogus1 : BOGUS1 =
    struct
        type t = int
 
        let f x y = x + y 
    end;;

module Bo  =
    struct
        type t = int
        let f x y = x + y
    end;;

