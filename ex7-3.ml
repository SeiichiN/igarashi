(* 練習問題 7.3 *)

(*
以下の関数 change は、お金を「くずす」関数です。

# let rec change coins amount =
    match (coins, amount) with
      (_, 0) -> []
    | ((c :: rest) as coins, total) ->
        if c > total then change rest total
        else c :: change coins (total - c);;

Characters 34-198:
Warning P: this pattern-matching is not exhaustive.
Here is an example of a value that is not matched.
([], 1)
  ..match (coins, amount) with
      (_, 0) -> []
    | ((c :: rest) as coins, total) ->
        if c > total then change rest total
        else c :: change coins (total - c)..
val change : int list -> int list = <fun>

与えられた（降順に並んだ）通過のリスト coins と合計金額 total からコインのリストを返します。

# let us_coins = [25; 10; 5; 1] (* アメリカのコイン *)
  and gb_coins = [50; 20; 10; 5; 2; 1] (* イギリスのコイン *) ;;
val us_coins : int list = [25; 10; 5; 1]
val gb_coins : int list = [50; 20; 10; 5; 2; 1]
# change gb_coins 43;;
- : int list = [20; 20; 2; 1]
# change us_coins 43;;
- : int list = [25; 10; 5; 1; 1; 1]

しかし、この定義は先頭にあるコインをできる限り使おうとするため、可能なコインの組み合わせがあるときにでも失敗してしまうことがあります。

# change [5; 2] 16;;
Exception: Match_failure ("", 143, 2).

これを例外処理を用いて解がひとつでも存在する場合には答えが得られるようにすることを考えます。以下の2箇所の ... を埋めて関数定義を完成させなさい。

let rec change coins amount =
    match (coins, amount) with
      (_, 0) -> []
    | ((c :: rest) as coins, total) ->
        if c > total then change rest total
        else
            (try
                c :: change coins (tatal - c)
             with Failure "change" -> ... )
    | _ -> ... ;;

# change [5; 2] 16;;
- : int list = [5; 5; 2; 2; 2]

 *)
(*
let rec change coins amount =
  match (coins, amount) with
    (_, 0) -> []
  | ((c :: rest) as coins, total) ->
     if c > total then change rest total
     else c :: change coins (total - c);;
 *)

(*
exception Failure of string;;
 *)

let rec change coins amount =
    match (coins, amount) with
      (_, 0) -> []
    | ((c :: rest) as coins, total) ->
        if c > total then change rest total
        else
            (try
                c :: change coins (total - c)
              with Failure "change" -> change rest total )
    | _ -> raise (Failure "change")
;;

let us_coins = [25; 10; 5; 1] (* アメリカのコイン *)
and gb_coins = [50; 20; 10; 5; 2; 1] (* イギリスのコイン *) ;;

let test1 = change gb_coins 43 = [20; 20; 2; 1];;
let test2 = change us_coins 43 = [25; 10; 5; 1; 1; 1];;

let test11 = change [5; 2] 16 = [5; 5; 2; 2; 2];;

  (*

let rec change coins amount =
  match (coins, amount) with
    (_, 0) -> []
  | (_, 1) -> [1]
  | ((c :: rest) as coins, total) ->
     if c > total then change rest total
     else c :: change coins (total - c);;
   *)

let us_coins = [25; 10; 5; 1] (* アメリカのコイン *)
and gb_coins = [50; 20; 10; 5; 2; 1] (* イギリスのコイン *) ;;

let my_coins = [5; 2];;

let test21 = change gb_coins 43 = [20; 20; 2; 1];; 
let test22 = change us_coins 43 = [25; 10; 5; 1; 1; 1];;
let test23 = change my_coins 43 = [5; 5; 5; 5; 5; 5; 5; 2; 2; 2; 2];;
let test24 = change [5; 2] 16 = [5; 5; 2; 2; 2];;
  
