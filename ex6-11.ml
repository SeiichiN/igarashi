(* 練習問題 6.11 *)

(*
上の arith から、その数式の文字列表現を求める関数 string_of_arith、分配則を用いて数式を (i11 X ... X i1n1_ + ... + (im1 X ... X imnm) （ただし ixy は整数）の形に変形する関数 expand を定義しなさい。

# string_of_arith exp;;
- : string = "((3 + 4) * (2 + 5))"
# string_of_arith (expand exp);;
- : string = "(((3 * 2) + (3 * 5)) + ((4 * 2) + (4 * 5)))"

簡単だと思う人は、string_of_arith の出力結果のカッコを減らすように工夫しなさい。
*)
