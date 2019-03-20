(* 練習問題 6.9 *)

以下のヴァリアントは、XML文書の構成要素を表すデータ構造です。（PCDATAというのは、XMLの用語での文字列です）。

(*
# type token = PCDATA of string | Open of string | Close of string;;
type token = PCDATA of string | Open of string | Close of string
*)

これで（整形式ではないかもしれない）XML文書を token のリストで表すことができます。例えば、

(*
<a>
  <b></b>
  <c>Hello</c>
</a>
*)

は、 token のリストを使って

(*
# [ Open "a"; Open "b"; Close "b";
Open "c"; PCDATA "Hello"; Close "c"; Close "a" ];;
- : token list =
    [Open "a"; Open "b"; Close "b"; Open "c"; PCDATA "Hello"; Close "c"; Close "d"]
*)

と表現できます。 token list を受け取って、それが整形式 XML を表しているなら (string, string）XMLを返す関数 xml_of_tokens を定義しなさい。（整形式ではない場合の動作についてはここでは特に指定しません）。


