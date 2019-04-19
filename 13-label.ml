(* 13 ラベル付き引数とオプション引数 *)

(* 13.1 ラベル付き引数を使う *)

(* fold_left の見本 *)
let rec fold_left f e = function
    [] -> e
    | v :: rest ->
        fold_left f (f v e) rest
;;
(* val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun> *)

let test1 = fold_left (fun x y -> x + y) 0 [1; 2; 3; 4] = 10;;

(* Listi モジュール *)
List.fold_left;;
  (* - : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun> *)
  
(* ListLabels モジュール *)
ListLabels.fold_left;;
  (* - : f:('a -> 'b -> 'a) -> init:'a -> 'b list -> 'a = <fun> *)
    
(* 例 *)
ListLabels.fold_left ~f:(fun x y -> x + y) ~init:0 [1; 2; 3; 4];;

(* 引数の順番を大幅に変えてもＯＫ *)
ListLabels.fold_left [1; 2; 3; 4] ~init:0 ~f:(fun x y -> x + y) ;;

(* レコードを引数にとる *)
type ('a, 'b) foldarg = {f: 'a -> 'b -> 'a; init: 'a};;

let rec fold_left' {f=f; init=init} = function
    [] -> init
    | a :: rest ->
            fold_left' {f = f; init = f init a} rest;;

fold_left' {f = (fun x y -> x + y); init = 0} [1; 2; 3; 4];;
fold_left' {init = 0; f = (fun x y -> x + y); } [1; 2; 3; 4];;

let g = ListLabels.fold_left ~init:0;;

g ~f:(fun x y -> x + y) [1; 2; 3; 4];;

ListLabels.fold_left ~f:(fun x y -> x + y);;

(****************** 13.2 ラベル付き引数詳説 ******************)
(* --------- ラベル付き引数宣言 ---------- *)

let rec fold_left ~f:func ~init:e = function
    [] -> e
    | a :: rest ->
            fold_left ~f:func ~init:(func e a) rest;;

(* val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b list -> 'a = <fun> *)

(* ラベル名と仮引数名が同じ場合、省略できる ~f=f ~init=init *)
let rec fold_left ~f ~init = function
    [] -> init
    | a :: rest ->
            fold_left ~f ~init:(f init a) rest;;

(* val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b list -> 'a = <fun> *)

let foo ~x:y:int = y;;  (* x ラベル名、y 仮引数、int yの型 *)

let foo' ~(x:int) = x;;  (* ラベル名と仮引数名が同じ場合 *)

(* -------- 適用時の引数順序の入れ替えとラベルの省略 -------- *)

ListLabels.map;;
(* - : f:('a -> 'b) -> 'a list -> 'b list = <fun> *)

ListLabels.map (fun x -> x + 1) [1; 2; 3; 4];; (* 実引数のラベルを省略 *)

let k ~const ~ignored = const;;  (* Kコンビネータ *)
(* val k : const:'a -> ignored:'b -> 'a = <fun> *)

let k' = k 1 2;;
(* val k' : const:(int -> int -> 'a) -> ignored:'b -> 'a = <fun> *)
(* k 1 2 == const:(int -> int -> 'a) *)

k' ~const:(fun x y -> x) ~ignored:"hoge";;


(* -------------- ラベル付き関数を受け取る関数 ------------ *)
let apply f arg1 arg2 = f ~arg1 ~arg2;;
(* val apply : (arg1:'a -> arg2:'b -> 'c) -> 'a -> 'b -> 'c = <fun> *)

apply (fun ~arg1 ~arg2 -> arg1 * arg2 + 1) 4 7;;
(* - : int = 29 *)

(*********** 13.3 オプション引数 ***************)
(* ------- 13.3.1 オプション引数の宣言 ---------- *)

(* （例）
 * seq: 整数の列をリストとして生成する関数
 *
 *   from -- 列の最初の数
 *   ?step:(s=1) -- 増分（隣同士の数の差）
 *   n -- 生成する要素の数
 *)
let rec seq from ?step:(s=1) n =
    if n <= 0 then []
    else from :: seq (from + s) ~step:s (n - 1);;
(* val seq : int -> ?step:int -> int -> int list = <fun> *)

(*
 * オプション引数
 *   ? ラベル名 : ( パターン : 型 = 式 )
 *   ? ラベル名 : ( パターン = 式 )
 *       式 -- その引数のデフォルト値
 *)

let rec seq from ?(step=1) n =
    if n <= 0 then []
    else from :: seq (from + step) ~step (n - 1);;

(* ---------- 13.3.2 引数の省略 ----------- *)

(* オプション引数に値を与える場合は、必ずラベルをつける *)
seq 5 ~step:2 3;;  (* [5; 7; 9] *)

seq 3 10;;  (* [3; 4; 5; 6; 7; 8; 9; 10; 11; 12] *)
(* ~step引数がない => ラベル付き引数が省略された *)

(* seq 5 2 3;; *) (* ラベルがないと、エラーになる *)
(*
Error: The function applied to this argument has type ?step:int -> int list
This argument cannot be applied without label
*)

(********************************************************************
 * オプション引数が引数の最後にある、あるいは、その後の引数すべてにラベルがついていたら、オプション引数を省略できない。
 * オプション引数のあとには（ダミーの unit型の引数でもいいので）ラベル無し引数を用意する必要がる。
 ********************************************************************)

(* オプション引数の位置は変更できる *)
seq 1 10 ~step:4;;  (* [1; 5; 9; 13; 17; 21; 25; 29; 33; 37] *)

(* ----------- 13.3.3 オプション引数の正体 ----------- *)

