(* 練習問題 3-15 *)

(* int -> int -> int -> int *)
let cube x = (fun y ->
              (fun z ->
                     x * y * z
              )
             );;

let test1 = cube 2 3 4 = 24;;
  
let daikei a b h = (a + b ) * h / 2;;

let test2 = daikei 2 3 4 = 10;;


  (* (int -> int) -> int -> int *)

let rec sum_of f n =
  if n = 0 then 0
  else
    sum_of f (n-1) + f n;;

let square x = x * x;;

let test3 = sum_of square 4 = 30;;

  (* (int -> int -> int) -> int *)
let sikaku a b = a * b;;

let test4 = sikaku 2 4 = 8;;

let risoku ritsu moto = moto *. ritsu;;

let test5 = risoku 0.03 1000. = 30.;;

let rec hukuri f ritsu moto years =
  if years = 0 then 0.
  else
    let moto = f ritsu moto +. moto in
    floor(hukuri f ritsu moto (years - 1) +. moto);;
  
let test6 = hukuri risoku 0.03 10000. 3 = 31836.;;

let cube x = x * x * x;;

let test7 = cube 2 = 8;;

let rec piramid f x h =
  if h = 0 then 0
  else
    piramid f x  (h-1) + (f x) * h * h;;

let test8 = piramid cube 2 4 = 240;;
              
let rec goukei f =
  (fun a ->
   (fun b -> 
    if b = 0 then 0
    else
      goukei f a (b-1) + f a b
   ));;
  
let rec rippo a b c =
  if c = 0 then 0
  else
    rippo a b (c-1) + sikaku a b * c;;
  
  
  
let add_one a b =
   sikaku a b + 1;;

  
