(* 練習問題 5.7 *)

(*
 * 与えられた自然数 r に対して、x*x + y*y = r であるような (x, y) （ただし x >= 0, y >= 0）の組すべてをリストとして列挙する関数 squares r を定義しなさい。
 * （検算用資料： r = 48612265 の時、32個の解があるそうです）。
 *)

(*
let rect x y = x*x + y*y;;

let test1 = rect 2 3 = 13;;

let sec limit =
    let rec kei x y =
        let d_x = x in
        if x*x + y*y <= limit
    then kei x+1 y
    else kei d_x y+1
    in
    kei 1 1;;
*)


let squares r =
    let rec keisan (x, y) ac =
        let def_x = x in
        let res = x * x + y * y in
        if res <= r
        then
            if res = r
            then keisan (x+1, y) ((x, y) :: ac)
            else keisan (x+1, y) ac
        else
            let x = def_x in
            if res <=r
            then
                if res =r
                then keisan (x, y+1) ((x, y) :: ac)
                else keisan (x, y+1) ac
            else
            ac
    in
    keisan (1, 1) [];;


