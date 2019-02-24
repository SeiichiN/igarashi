(* 練習問題 4.5 *)

(*
 * twice twice f x が f(f(f(fx))) として働く理由を計算ステップを示すことで説明せよ。
 *)

let twice f x = f (f x);;

let addten x = x + 10;;

let test1 = addten 0 = 10;;

let test2 = twice addten 3 = 23;;

let test3 = twice twice addten 0 = 40;;
(* twice twice addten 0 -> twice addten (addten 0) ->
    addten (addten (addten (addten 0))) -> addten (addten (addten 10)) ->
    addten (addten 20) -> addten 30 -> 40
* ) 
