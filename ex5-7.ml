(* 練習問題 5.7 *)

(*
 * 与えられた自然数 r に対して、x*x + y*y = r であるような (x, y) （ただし x >= 0, y >= 0）の組すべてをリストとして列挙する関数 squares r を定義しなさい。
 * （検算用資料： r = 48612265 の時、32個の解があるそうです）。
 *)

(*
 * リスト l は、(x, y)の組のリストである。
 * その中に、(x, y) = (y, x) であるリストは重複しているとして省きたい。
 * そのため、組(a, b) について、 リストの中に重複しているものがあるかどうかを調べる関数
 *)
let rec find (a, b) l =
    match l with
        [] -> false
    | (x, y) :: rest ->
            if ((b, a) = (x, y))
            then true
            else find (a, b) rest;;

let squares r =
    let rec keisan (x, y) ac =
        let res = x * x + y * y in
        if res <= r
        then
            if res = r
            then
                if find (x, y) ac
                then keisan (x+1, y) ac
                else keisan (x+1, y) ((x, y) :: ac)
            else keisan (x+1, y) ac
        else
            if y*y < r
            then keisan (1, y+1) ac
            else
                ac
    in
    keisan (1, 1) [];;


