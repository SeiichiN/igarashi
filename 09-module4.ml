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

module Table1 :
    (* シグネチャの定義 *)
    sig
        type ('a, 'b) t = Empty | Entry of 'a * 'b * ('a, 'b) t

        val empty : ('a, 'b) t

        val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

        val retrieve : 'a -> ('a, 'b) t -> 'b option

        val dump : ('a, 'b) t -> ('a * 'b) list
    end
=
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


(* さっきと同じように使ってみる *)
let ( <<< ) table (key, content) = Table1.add key content table;;

let table = Table1.empty
<<< ("a", "the first letter of the English alphabet")
<<< ("b", "the second letter of the English alphabet")
<<< ("zzz", "sleeping noise");;

let table' = table <<< ("a", "an indefinite article");;

(Table1.retrieve "a" table', Table1.dump table');;

(* 削除してみる *)
(*
Table1.delete "a" table';;  (* 想定外の使い方 *)
*)
(* Error: Unbound value Table1.delete というエラーが出る *)
