(* 8.3 制御構造 *)

let () = print_string "Hello, " in
print_string "World!\n";;

let f x y = 1 in
f (print_string "Hello, ") (print_string "World\n");;

(print_string "Hello, ", print_string "World\n");;

ignore;;

let print_hello () = print_string "Hello, "; 0;;

print_hello (); print_string "World!\n";;

ignore (print_hello ()); print_string "World!\n";;

(print_string "Hello, "; print_string "World!\n";);;

let print_hello s =
    print_string "Hello, ";
    print_string s;
    print_newline ();;

let print_bye s =
    print_string "Bye bye, ";
    print_string s;
    print_newline ();;

(* 8.3.2 条件分岐 *)

let f1 b = if b then print_string "a"; print_string "b\n";;

let f2 b = (if b then print_string "a"); print_string "b\n";;

if false then begin print_string "a"; print_string "b" end else ();;

(* 8.3.3 繰り返し *)

(* 命令的な階乗関数の定義 *)
let fact n =
    let i = ref 1
    and res = ref 1 in
    while (!i <= n) do
        res := !res * !i;
        i := !i + 1
    done;
    !res;;

let i = ref 1 and res = ref 1;;
!i = !res;;   (* true *)
!i == !res;;  (* true *)
i = res;;     (* true *)
i == res;;    (* false *)

(* 再帰的な階乗関数
let rec fact n =
    if n = 1 then 1
    else fact (n-1) * n;;
*)

(* キーボードから入力された行を二つつなげて画面に出力する関数 *)
let parrot () =
    let s = ref "" in
    while (s := read_line (); !s <> ".") do
        print_string !s;
        print_endline !s
    done;;

(* while を再帰関数であらわす *)
let rec whle condition body =
    if condition () then
        begin body (); whle condition body end;;
(* 条件部・本体部を関数とすることで、適用のたびに違った値が返ってくる可能性が
 * 生まれる。
 *)

(* fact を whle を使って記述 *)
let fact n =
    let i = ref 1 and res = ref 1 in
    whle (fun () -> (!i <= n))
    (fun () -> res := !res * !i; i := !i + 1);
    !res;;

(* iter リストの各要素に命令を表す関数を順に適用する *)
let rec iter f = function
    [] -> ()
    | a :: rest ->
            begin
                f a;
                iter f rest
            end;;

(* 駅名の入った文字列リストの内容を画面に表示する *)
iter (fun s -> print_string "Station: "; print_endline s)
["Tokyo"; "Shinagawa"; "Shin-Yokohama"; "Nagoya"; "Kyoto"; "Shin-Osaka"];;

