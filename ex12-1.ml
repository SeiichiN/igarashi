(* プログラミング in OCaml 練習問題 12.1 *)

(* 練習問題 12.1 *)
(*
以下の calc の定義のおかしな点を指摘せよ。

# class calc =
    object
        val mutable num = 0
        val mutable func = fun x -> x
        
        method input n = num <- n
        method plus = func <- (fun y -> num + y)
        method eq = func num
    end;;

class calc :
  object
    val mutable func : int -> int
    val mutable num : int
    method eq : int
    method input : int -> unit
    method plus : unit
  end
*)

class calc =
    object (s)
        val mutable num = 0
        val mutable func = fun x -> x
        
        method input n = num <- n
        method plus = 
            let x = num in
            func <- (fun y -> x + y);
            num <- 0
        method eq = func num
    end;;

let my = new calc;;
my#input 11; my#plus; my#input 23; my#eq;;

