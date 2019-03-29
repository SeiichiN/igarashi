(* 書き換え可能なデータ構造 *)

let s = "life";;
s.[2] <- 'k';;
s;;

let pair = ("life", 2);;
(fst pair).[2] <- 'k';;
pair;;

let pair = ("life", "life");;
(fst pair).[2] <- 'k';;
pair;;

let pair = let p = "life" in (p, p);;
(fst pair).[2] <- 'k';;
pair;;
          
let s = "life";;
let pair1 = ("life", s);;
let pair2 = (s, s);;

(pair1 = pair2, fst pair1 = fst pair2, snd pair1 = snd pair2);;
(pair1 == pair2, fst pair1 == fst pair2, snd pair1 == snd pair2);;

let update_string s1 s2 =
    let () = s1.[0] <- 'a' in
    let () = s2.[0] <- 'b' in
    s1.[0] = s2.[0];;

update_string "xyz" "xyz";;

let s = "xyz" in update_string s s;;

type teacher = {name : string; mutable office : string};;

let t = {name = "Igarashi"; office = "140"};;

