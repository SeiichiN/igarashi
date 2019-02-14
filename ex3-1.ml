(* 練習問題 3.1 *)

(* 1 *)
(* usドル（実数）を受け取って、円（整数）に換算する関数を書け *)
(* ただし、1円以下は四捨五入する *)
(* 1$ = 114.32円 とする *)

let rate = 114.32;;
  
let dol2yen dol = int_of_float(floor ( dol *. rate +. 0.5));;
  
let yen2dol yen = floor((float_of_int(yen) /. rate) *. 100. +. 0.5) /. 100.;;
  
let kanzan_dol2yen (dol:float) =
  let str_dol_up = string_of_int(int_of_float(floor dol)) in
  let str_dol_low = string_of_int(int_of_float(floor (dol *. 100.)) mod 100) in
  let str_yen = string_of_int(dol2yen dol) in
  str_dol_up ^ "." ^ str_dol_low ^ " dollers are " ^ str_yen ^ " yen.";;

  (* TEST *)
let test1 = dol2yen 12. = 1372
let test2 = dol2yen 8.32 = 951
let test3 = yen2dol 1200 = 10.5
let test4 = yen2dol 1300 = 11.37
let test5 = kanzan_dol2yen 12.34 = "12.34 dollers are 1411 yen."
let test6 = kanzan_dol2yen 134.25 = "134.25 dollers are 15347 yen."
                                      

let capitalize moji =
  let int_moji = int_of_char moji in
  if (int_moji >= 97 && int_moji <= 122 )
  then char_of_int(int_moji - 32)
  else moji
  

  (* TEST *)
let test7 = capitalize 'b' = 'B'
let test8 = capitalize 'C' = 'C'
                                
                                

