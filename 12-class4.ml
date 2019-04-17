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
    
