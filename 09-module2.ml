(* 9.3 シグネチャを使った情報隠蔽と抽象データ型 *)

(* table テーブル -- 見出し語：解説文 *)

(* 定義
 *
 * empty -- 空のテーブル
 * add   -- 既存のテーブルに新しいエントリを追加
 *          （同じキーが使われていたら、上書き）
 * retrieve -- キーからデータを検索する
 * dump  -- テーブルの内容をキーとデータの組のリストとして返す
 *)

module Table =
    struct
        type ('a, 'b) t = Empty | Entry of 'a * 'b * ('a, 'b) t

        let empty = Empty

        let add key datum table = Entry (key, datum, table)

        let rec retrieve key = function
            Empty -> None
            | Entry (key', datum, rest) ->
                    if key = key' then Some datum
                    else retrieve key rest

        let rec delete key = function
            Empty -> Empty
            | Entry (key', datum, rest) ->
                    if key = key' then delete key rest
                    else Entry (key', datum, delete key rest)

        let rec dump = function
            Empty -> []
            | Entry (key, contents, rest) ->
                    (key, contents) :: (dump (delete key rest))
    end;;

(* 使用例 *)
let ( <<< ) table (key, content) = Table.add key content table;;

let table = Table.empty
<<< ("a", "the first letter of the English alphabet")
<<< ("b", "the second letter of the English alphabet")
<<< ("z", "sleeping noise");;

Table.retrieve "a" table;;
(* - : string option = Some "the first letter of the English alphabet"  *)

(* キー "a" のエントリを上書きする *)
let table' = table <<< ("a", "an indefinite article");;

(* "a" を検索してみる -- テーブルは table' *)
Table.retrieve "a" table';;
(* - : string option = Some "an indefinite article" *)

(* テーブルの内容を表示する *)
Table.dump table';;
(*
- : (string * string) list =
    [("a", "an indefinite article"); ("z", "sleeping noise");                             ("b", "the second letter of the English alphabet")]   
重複するキーのエントリは表示されない（delete関数のはたらき）
*)

(* 削除してみる *)
Table.delete "a" table';;  (* 想定外の使い方 *)
(*
- : (string, string) Table.t =
    Table.Entry ("z", "sleeping noise",
    Table.Entry ("b", "the second letter of the English alphabet", Table.Empty))
"a"のエントリは削除されたかにみえるが・・・
*)

table';;
(*
- : (string, string) Table.t =
Table.Entry ("a", "an indefinite article",
 Table.Entry ("z", "sleeping noise",
  Table.Entry ("b", "the second letter of the English alphabet",
   Table.Entry ("a", "the first letter of the English alphabet", Table.Empty))))
実際は、削除されてない。表示されないだけ。
*)

(*****************************************************************************
 * << シグネチャ >>
 *
 * モジュールの想定される使い方を記述した、モジュールのインターフェースを
 * プログラムの一部として書く
 *
 * 想定外の使い方をした場合にそれを実行する前に検査する
 *****************************************************************************)

(* シグネチャの定義 *)
module type TABLE1 =

    sig
        type ('a, 'b) t = Empty | Entry of 'a * 'b * ('a, 'b) t

        val empty : ('a, 'b) t

        val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

        val retrieve : 'a -> ('a, 'b) t -> 'b option

        val dump : ('a, 'b) t -> ('a * 'b) list
    end;;

(* モジュールにシグネチャを与える *)
module Table1 : TABLE1 = Table;;

(* さっきと同じように使ってみる *)
let ( <<< ) table (key, content) = Table1.add key content table;;

let table = Table1.empty
<<< ("a", "the first letter of the English alphabet")
<<< ("b", "the second letter of the English alphabet")
<<< ("zzz", "sleeping noise");;

let table' = table <<< ("a", "an indefinite article");;

(Table1.retrieve "a" table', Table1.dump table');;

(* 削除してみる *)
Table1.delete "a" table';;  (* 想定外の使い方 *)
(* Error: Unbound value Table1.delete というエラーが出る *)
