(* 練習問題 5.2 *)

(*
1. 正の整数 n から 1 までの整数の降順リストを生成する関数 downto1 を定義せよ。
*)

let rec downto1 n =
  if n = 1 then [1]
  else
    n :: downto1 (n-1) ;;

let test1 = downto1 6 = [6; 5; 4; 3; 2; 1];;
  
(*
2. 与えられた正の整数のローマ字表現（文字列）を求める関数 roman。
I = 1, V = 5, X = 10, L = 50, C = 100, D = 500, M = 1000
ただし、roman は、単位となる数とローマ数字表現の組を大きいものから並べたリストで
表現した、ローマ数字の定義も引数として受け取ることにする。
たとえば、
# roman [(1000, "M"); (500, "D"); (100, "C"); (50, "L"); (10, "X"); (5, "V"); (1, "I")] 1984;;
- : string = "MDCCCCLXXXIIII"
となる。
また、4, 9, 40, 90, 400, 900 などの表現にも注意して、
# roman [(1000, "M"); (900, "DM"); (500, "D"); (400, "CD"); (100, "C"); (90, "XC"); 
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
  (if ((n / 1000) > 0)
   then pick_roman "M" (n / 1000)
   else "")
  ^
    (let n2 = n mod 1000 in 
     if (n2 / 900) = 1
     then "CM"
     else pick_roman "D" (n2 / 500))
  ^
    (let n2 = ((n mod 1000) mod 900) mod 500 in        
     if (n2 / 100) = 4
     then "CD"
     else pick_roman "C" (n2 / 100))
  ^
    (let n2 = n mod 100 in
     if (n2 / 90) = 1
     then "XC"
     else pick_roman "L" (n2 / 50))
  ^
    (let n2 = ((n mod 100) mod 90) mod 50 in
     if (n2 / 10) = 4
     then "XL"
     else pick_roman "X" (n2 / 10))
  ^
    (let n2 = n mod 10 in
     if (n2 / 9) = 1
     then "IX"
     else pick_roman "V" (n2 / 5))
  ^
    (let n2 = ((n mod 10) mod 9) mod 5 in
     if n2 = 4
     then "IV"
     else pick_roman "I" n2);;

let test83 = roman romanlist1 1984 = "MCMLXXXIV";;
let test84 = roman romanlist1 1987 = "MCMLXXXVII";;

let test85 = roman romanlist2 1984 = "MCMLXXXIV";;
let test86 = roman romanlist2 1987 = "MCMLXXXVII";;

let test87 = roman romanlist2 1900 = "MCM";;
let test88 = roman romanlist2 1800 = "MDCCC";;
let test88 = roman romanlist2 1700 = "MDCC";;
let test88 = roman romanlist2 1600 = "MDC";;
let test88 = roman romanlist2 1500 = "MD";;
let test88 = roman romanlist2 1400 = "MCD";;
let test88 = roman romanlist2 1300 = "MCCC";;
let test89 = roman romanlist2 89 = "LXXXIX";;
  
  
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

let renketsu a b =
  (if a = 1
  then "A"
  else "B")
       ^
         (if b = 1
         then "X"
         else "Y");;

let test101 = renketsu 1 1 = "AX";;
let test102 = renketsu 1 2 = "AY";;
let test103 = renketsu 0 1 = "BX";;
let test104 = renketsu 2 1 = "BX";;
let test105 = renketsu 2 2 = "BY";;
  
  
