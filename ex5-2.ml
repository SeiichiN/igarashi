(* 練習問題 5.2 *)

(* 1. 正の整数 n から 1 までの整数の降順リストを生成する関数 downto1 を定義せよ。 *)

let rec downto1 n =
  if n = 1 then [1]
  else
    n :: downto1 (n-1) ;;

let test1 = downto1 6 = [6; 5; 4; 3; 2; 1];;
  
(* 2. 与えられた正の整数のローマ字表現（文字列）を求める関数 roman。
I = 1, V = 5, X = 10, L = 50, C = 100, D = 500, M = 1000
ただし、roman は、単位となる数とローマ数字表現の組を大きいものから並べたリストで
表現した、ローマ数字の定義も引数として受け取ることにする。
たとえば、
# roman [(1000, "M"); (500, "D"); (100, "C"); (50, "L"); (10, "X"); (5, "V"); (1, "I")] 1984;;
- : string = "MDCCCCLXXXIIII"
となる。
また、4, 9, 40, 90, 400, 900 などの表現にも注意して、
# roman [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD"); (100, "C"); (90, "XC"); 
(50, "L"); (40, "XL"); (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")] 1984;;
- : string ="MCMLXXXIV"
となるようにせよ。
 *)
  
let romanlist1 = [(1000, "M"); (500, "D"); (100, "C"); (50, "L"); (10, "X"); (5, "V"); (1, "I")];;

let romanlist2 = [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD"); (100, "C"); (90, "XC"); 
                 (50, "L"); (40, "XL"); (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")];;
  
  
let rec assoc a = function
    [] -> "Nothig"
   |(a', b) :: rest -> if a = a' then b else assoc a rest;;

let test81 = assoc 1000 romanlist1 = "M";;

(*
* pick_roman -- n 回 moji を連続表示する
* moji -- string ex."M", "D" ...
* n    -- int number
*)
let rec pick_roman moji n =
  if n = 0 then ""
  else
    moji ^ pick_roman moji (n-1);;

let test82 = pick_roman "M" 2 = "MM";;
  
(*
 * l -- list .. romanlist
 * n -- number .. ex.1984
 * return -- string .. ex."MDCCCLXXXIII"
 *)

let roman l n =
  pick_roman "M" (n / 1000) ^
     if ((n mod 1000) / 900) = 1
     then "CM"
     else pick_roman "D" ((n mod 1000) / 500) ^
         if ((n mod 500) / 100) = 4
         then "CD"
         else pick_roman "C" ((n mod 500) / 100) ^
             if ((n mod 100) / 90) = 1
             then "CL"
             else pick_roman "L" ((n mod 100) / 50) ^
                 if ((n mod 50) / 10) = 4
                 then "IX"
                 else pick_roman "X" ((n mod 50) / 10) ^
                     if ((n mod 10) / 9) = 1
                     then "IV"
                     else pick_roman "V" ((n mod 10) / 5) ^
                         pick_roman "I" (n mod 5);;

let test83 = roman romanlist1 1984 = "MDCCCCLXXXIIII";;
let test84 = roman romanlist1 1987 = "MDCCCCLXXXVII";;

let test85 = roman romanlist2 1984 = "MCMLXXXIV";;
let test86 = roman romanlist2 1987 = "MCMLXXXVII";;

let test87 = roman romanlist2 1900 = "MCM";;
let test88 = roman romanlist2 1800 = "MDCCC";;
let test88 = roman romanlist2 1700 = "MDCC";;
let test88 = roman romanlist2 1600 = "MDC";;
let test88 = roman romanlist2 1500 = "MD";;
let test88 = roman romanlist2 1400 = "MCD";;
let test88 = roman romanlist2 1300 = "MCCC";;
  
  
(*
let roman l n =
  let m = n / 1000 when n / 1000 > 0 in
  let d = (n mod 1000) / 500 when (n mod 1000) / 500 > 0 in
  let c = (n mod 500) / 100 when (n mod 500) / 100 > 0 in
  let l = (n mod 100) / 50 when (n mod 100) / 50 > 0 in
  let x = (n mod 50) / 10 when (n mod 50) / 10 > 0 in
  let v = (n mod 10) / 5 when (n mod 10) / 5 > 0 in
  let i = (n mod 5) when n mod 5 > 0 in
  assoc 1000
 *)

