(* プログラミング in OCaml 練習問題 8.7 *)

(* 練習問題 8.7 *)

(*
リストに対する繰り返し iter を参考にして、配列要素に対して繰り返し処理を行う array_iter を定義しなさい。
（ヒント：配列の長さの求め方がわからなければ、長さを越えた要素アクセスで例外が発生することを利用しなさい）。
 *)                     

(* 出力例
# array_iter;;
  - : ('a -> 'b) -> 'a array -> unit = <fun>

# array_iter (fun s -> print_string "Station: "; print_endline s)
    {|"Tokyo"; "Shinagawa"; "Shin-Yokohama"; "Nagoya"; "Kyoto"; "Shin-Osaka"|};;

Station: Tokyo
Station: Shinagawa
Station: Shin-Yokohama
Station: Nagoya
Station: Kyoto
Station: Shin-Osaka
- : unit = 0
*)

(* リストに対する繰り返し iter *)
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

let ar = [| 1; 2 |];;
let [| b; c |] = ar;;

let rec array_iter f arr =
    let i = ref 1 in
    begin
        f arr.(!i);

