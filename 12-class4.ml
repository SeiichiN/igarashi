(* 12.5.3 多相クラス *)

let generic_calc init f =
  object
    val mutable num = init
    val mutable func = fun x -> x

    method input n = num <- n
    method op = func <- f num
    method eq = func num
  end;;
(*
val generic_calc :                                                                                        'a -> ('a -> 'a -> 'a) -> < eq : 'a; input : 'a -> unit; op : unit > = <fun> 
 *)
  
let f_calc = generic_calc 0.0 (fun x y -> x *. y) in
    f_calc#input 2.0; f_calc#op; f_calc#input 3.14; f_calc#eq;;
(*
- : float = 6.28
 *)

let f_calc = generic_calc "" (fun x y -> x ^ y) in
    f_calc#input "Hello, "; f_calc#op; f_calc#input "World!"; f_calc#eq;;
(*
- : string = "Hello, World!"
 *)

class virtual ['a] generic_calc init =
  object
    val mutable num = (init : 'a)
    val mutable func = fun x -> x

    method input n = num <- n
    method virtual op : unit
    method eq = func num
  end;;

class fcalc =
object
  inherit [float] generic_calc 0.0  (* 型変数を float で具体化する *)
  method op =
    let x = num in
    func <- (fun y -> x *. y)
end;;

let f_calc = new fcalc;;

f_calc#input 2.0; f_calc#op; f_calc#input 3.14; f_calc#eq;;
    
(f_calc :> float generic_calc);;
    

(* 多相メソッド *)

class ['a] olist init =
    object
        val mutable l = (init : 'a list)
        method cons a = l <- a :: l
        method length = List.length l
        method append l' = l <- List.append l l'
    end;;

let ol = new olist [1];;

ol#cons 3; ol#length;;

class ['a, 'b] olist_fold init =
    object
        inherit ['a] olist init
        method fold_right f (e : 'b) = List.fold_right f l e
    end;;

let l = new olist_fold [1; 2; 3];;

l#fold_right (fun x y -> float_of_int x +. y) 0.0;;

class ['a] olist_fold' init =
    object
        inherit ['a] olist init
        method fold_right : 'b.('a -> 'b -> 'b) -> 'b -> 'b =
            fun f e -> List.fold_right f l e
    end;;

let l = new olist_fold' [1; 2; 3];;

l#fold_right (fun x y -> float_of_int x +. y) 0.0;;

l#fold_right (fun x y -> string_of_int x ^ ";" ^ y) "";;

class ['a] olist_fold' init =
    object (self : <fold_right : 'b.('a -> 'b -> 'b) -> 'b -> 'b; ..>) 
        inherit ['a] olist init
        method fold_right f e = List.fold_right f l e
    end;;

let l = new olist_fold' [1; 2; 3];;

l#fold_right (fun x y -> float_of_int x +. y) 0.0;;

l#fold_right (fun x y -> string_of_int x ^ ";" ^ y) "";;
