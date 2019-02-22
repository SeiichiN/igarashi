(* chap.4 多相性と型推論 *)

let fst_int ((x, y) : int * int) = x;;

(* TEST *)
let test1 = fst_int (1, 3) = 1;;
    
let fst_ifs ((x, y) : (int * float) * string) = x;;

  (* TEST *)
let test2 = fst_ifs ((1, 3.), "abc") = (1, 3.);;

let fst (x, y) = x;;

  (* TEST *)
let test3 = fst (3, 4) = 3;;
let test4 = fst (4, 5.2) = 4;;
let test5 = fst (5, "abc") = 5;;

  (* 恒等関数 *)
let id x = x;;

let test6 = id 3 = 3;;
  
