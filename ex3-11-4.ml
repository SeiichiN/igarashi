(* 練習課題 3-11-4 *)
(* 与えられた文字列の中で、ASCIIコードが最も大きい文字を返す max_ascii関数を定義せよ。文字列の長さを取り出す String.length という関数を使ってもよい。 *)

String.length "Hello!";;

int_of_char 'A';;

  (* ふたつのchar型の引数をとり、ASCIIコードの大きいほうを返す関数 *)
let takeBig (a, b) =
  if (int_of_char a) >= (int_of_char b)
  then a
  else b;;

  (* TEST *)
let test1 = takeBig('a', 'b') = 'b';;
let test2 = takeBig('A', 'a') = 'a';;

let moji = "Hello!";;
let i = 0;;
let (a, b) = (moji.[i], moji.[i+1]);;

let test3 = takeBig(a, b) = b;;

(* 文字列の個々の文字の大きさ(ASCII)を比べ、一番大きい文字を返す関数 *)
(* hikaku : String, int, char => char *)
let rec hikaku (moji, i, a) =
  if i = String.length moji
  then a
  else
    let a = takeBig (a, moji.[i]) in
    hikaku (moji, i+1, a)

(* TEST *)
let test11 = hikaku ("abcde", 1, 'a') = 'e';;
  
  
let max_ascii moji =
  let rec hikaku (moji, i, a) =
    if i = String.length moji                          
    then a
    else
      let a = takeBig (a, moji.[i]) in
      hikaku(moji, i+1, a)
  in
  hikaku (moji, 1, moji.[0])
                  
  (* TEST *)
let test4 = max_ascii "abcde" = 'e';;
let test5 = max_ascii "Az" = 'z';;
  


