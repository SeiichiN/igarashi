(* 練習問題 8.5 *)

(*
8.2.4節で述べた［］への参照の例を実際に対話式コンパイラで試し、本文中に書いた挙動との違い、特に、参照の型を説明し、どのようにして、true を [1] にコンスしてしまうような事態の発生が防がれているか説明しなさい。
 *)

# let x = ref [];;

として、本文に書かれているようにコンスをつくると、

# (2 :: !x, true :: !x);;

Error: This expression has type int list but an expression was expected of type bool list
Type int is not compatible with type bool

というエラーが出る。

これは、「int list」なのに、「bool list」がきてる、というエラー（だと思う）。

これは、以下のようにすると、

# (true :: !x, 2 :: !x);;

Error: This expression has type bool list
but an expression was expected of type int list
Type bool is not compatible with type int

というエラーに変わる。すなわち、「bool list」なのに、「int list」がきてる、というエラー。

つまり、(..., ...)の中に、異なるタイプのリストを入れることができないのである。

しかし、以下のようにすれば、できる。

# let y = ref [];;
# (2 :: !x, true :: !y);;
- : int list * bool list = ([2], [true])

ということは、2 :: !x としたあとに、別の型をコンスすることはできないということになる。


