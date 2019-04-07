(* プログラミング in OCaml 練習問題 9.3 *)

(* 練習問題 9.3 *)

(*
以下は、書き換え可能データを使わないキューのシグネチャです。add などは要素を追加した新しいキューを返すようになっています。

# module type QUEUE =
    sig

        type 'a t

        exception Empty

        val empty: 'a t

        val add: 'a t -> 'a -> 'a t

        val take: 'a t -> 'a * 'a t

        val peek: 'a t -> 'a

    end;;

module type QUEUE =
    sig
        type 'a t
        exception Empty
        val empty : 'a t
        val add : 'a t -> 'a -> 'a t
        val take : 'a t -> 'a * 'a t
        val peek : 'a t -> 'a
    end

このシグネチャを持つモジュールを以下に述べる二種類の実現方法で定義しなさい。
ひとつめのモジュール Queue1 は、キューをリストで表現します。例えば、[1; 5; 4; 3; 2] は、キュー 1, 5, 4, 3, 2 を表現します。もうひとつの Queue2 は、待ち行列をリストのペアで表現します（同じキューの状態を表現するリストのペアは複数あり得ますが、キューが空でない限り常に一つ目のリストが空でないようにしなさい）。このような表現により、add、take ともに（平均）定数時間で行えます。以下に、モジュール定義の一部を示すので、完成させた上で、動作確認をしなさい。

module Queue1 : QUEUE =
    struct

        type 'a t = 'a list
        
        ...

        let peek = function [] -> raise Empty | x :: rest -> x

    end

module Queue2 : QUEUE =
    struct
        type 'a t = Queue of ('a list * 'a list)

        ...

        let peek = function
            Queue ([], _) -> raise Empty
            | Queue (x :: _, _) -> x
    end
 *)

module type QUEUE =
    sig

        type 'a t

        exception Empty

        val empty: 'a t

        val add: 'a t -> 'a -> 'a t

        (* 先頭要素と残りの組を返す *)
        val take: 'a t -> 'a * 'a t

        (* 先頭要素を返す *)
        val peek: 'a t -> 'a

    end;;

module Queue1 : QUEUE =
    struct

        type 'a t = 'a list

        exception Empty

        let empty = []
        
        let add l x = l @ (x :: [])

        let take = function
            [] -> raise Empty 
        | a :: rest -> (a, rest)

        let peek = function [] -> raise Empty | x :: rest -> x

    end;;


module Queue2 : QUEUE =
    struct
        type 'a t = Queue of ('a list * 'a list)

        exception Empty

        let empty = Queue ([], [])

        let add l x =
            match l with
            Queue ([], []) -> Queue ([x], [])
        | Queue (a, b) -> Queue (a, x :: b)

        let rec reverse = function
            [] -> []
        | v :: rest -> (reverse rest) @ (v :: [])

        let take = function
            Queue ([], _) -> raise Empty
        | Queue (x :: [], b) -> (x, Queue((reverse b), []))
        | Queue (x :: rest, b) -> (x, Queue(rest, b))

        let peek = function
            Queue ([], _) -> raise Empty
            | Queue (x :: _, _) -> x
    end;;

(* 実行例 -- Queue1 *)
let rec dump1 qlist1 =
    try
        let (v, rest) = Queue1.take qlist1 in
        v :: (dump1 rest)
    with Queue1.Empty -> [];;

let ( <<< ) l x = Queue1.add l x;;

let mylist1 = Queue1.empty;;
let mylist1 = mylist1 <<< "sachiko" <<< "seiichi";;
dump1 mylist1;;
(* - : string list = ["sachiko"; "seiichi"] *)
Queue1.peek mylist1;;
(* - : string = "sachiko" *)

(* 実行例 -- Queue2 *)
let rec dump2 qlist2 =
    try
        let (v, rest) = Queue2.take qlist2 in
        v :: (dump2 rest)
    with Queue2.Empty -> [];;

let ( <== ) l x = Queue2.add l x;;

let mylist2 = Queue2.empty;;
let mylist2 = mylist2 <== "sayuri" <== "seiichi";;
dump2 mylist2;;
(* - : string list = ["sayuri"; "seiichi"] *)
Queue2.peek mylist2;;
(* - : string = "sayuri" *)
