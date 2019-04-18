(* プログラミング in OCaml 練習問題 12.2 *)

(* 練習問題 12.2 *)

(*
この章でみた電卓の定義では、1+2+3 のような連続した演算をおこなうことができません。

c#input 1; c#plus; c#input 2; c#plus; c#input 3; c#eq;;

のような呼び出しで計算ができるように改造しなさい。
*)

class calc =
    object (s)
        val mutable num = 0
        val mutable func = fun x -> x
        
        method input n = num <- n
        method plus = 
            let x = func num in
            func <- (fun y -> x + y);
        method eq = func num
    end;;

let my = new calc;;
my#input 11; my#plus; my#input 23; my#plus; my#input 5; my#eq;;

