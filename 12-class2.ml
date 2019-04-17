(* 12.3 継承によるクラスの拡張 *)

class calc =
object
  (* インスタンス変数 *)
  val mutable num = 0
  val mutable func = fun x -> x

  (* メソッド定義 *)
  method input n = num <- n
  method plus =
    let x = num in
    func <- (fun y -> x + y)
  method eq = func num
end;;

let c = new calc;;
  
c#input 4; c#plus; c#input 2; c#eq;;

class calc_minus =
object
  inherit calc   (* calc の定義を継承 *)

  method minus =
    let x = num in
    func <- (fun y -> x - y)
end;;

let cm = new calc_minus;;

cm#input 13; cm#minus; cm#input 4; cm#eq;;
         

class calc_for_kids =
object
  inherit calc_minus

  method eq = max (func num) 0
end;;

let kc = new calc_for_kids;;

kc#input 4; kc#minus; kc#input 13; kc#eq;;
    

class calc_for_kids2 =
object
  inherit calc_minus as super

  method eq = max (super#eq) 0
end;;

let kc2 = new calc_for_kids2;;

kc2#input 4; kc2#minus; kc2#input 13; kc2#eq;;

(* クラスを定義せず、オブジェクトを直接つくる *)
let calc_obj =
  object
    val mutable num = 0
    val mutable func = fun x -> x

    method input n = num <- n
    method plus =
      let x = num in
      func <- (fun y -> x + y)
    method eq = func num
  end;;

let foo x = if x then calc_obj else c;;
  
  [calc_obj; c];;
    
class calc_double =
object (self)
  val mutable num = 0
  val mutable func = fun x -> x


  method input n = num <- n
  method plus =
    let x = num in
    func <- (fun y -> x + y)
  method eq = func num
  method double = self#plus; self#eq
end;;

  (* コアーション -- 型変換 coersion *)
  (* 式 : 型1 :> 型2 *)
  (* 式 :> 型2 *)
  
  [new calc; (new calc_double :> calc); (new calc_minus :> calc)];;
    
(********** 12.4.3 多相的オブジェクト型 *******************)

let test_calc c =
  c#input 10; c#plus; c#input 20; c#eq = 30 in
test_calc (new calc) && test_calc (new calc_for_kids);;

let test_calc c =
  c#input 10; c#plus; c#input 20; c#eq = 30;;

(* val test_calc : < eq : int; input : int -> 'a; plus : 'b; .. > -> bool = <fun>  *)
  
let test_calc c =
  c#input 10; c#plus; c#input 20; c#eq = 30 in
test_calc (new calc);;

(* クラスの名前に # をつける -- ".."を含む多相的オブジェクトの別名 *)
  
let input_ten (c : #calc) = c#input 10;;
  (* val input_ten : #calc -> unit = <fun> *)
  (* #calc -- <eq : int; input : int -> unit; plus : unit; ..>  *)


  input_ten (new calc_for_kids);;

  let myobj =
    object
      val mutable num = 0

      method input n = num <- n
    end;;
(*
input_ten (myobj);;

Error: This expression has type < input : int -> unit >
       but an expression was expected of type #calc
       The first object type has no method eq
 *)

(* 12.4.4 さらに多相的オブジェクト型について *)

let input_ten calc = calc#input 10
and push_plus calc = calc#plus;;
(* val input_ten : < input : int -> 'a; .. > -> 'a = <fun>
 * val push_plus : < plus : 'a; .. > -> 'a = <fun> *)

let f b = if b then input_ten else push_plus;;
(* val f : bool -> < input : int -> 'a; plus : 'a; .. > -> 'a = <fun> *)

let k1 a b = a
and k2 a b = b;;
(* val k1 : 'a -> 'b -> 'a = <fun>
 * val k2 : 'a -> 'b -> 'b = <fun> *)

let f b = if b then k1 else k2;;
(* val f : bool -> 'a -> 'a -> 'a = <fun> *)

let input_foo c = c#input "foo";;
(* val input_foo : < input : string -> 'a; .. > -> 'a = <fun> *)

(* let f b = if b then input_ten else input_foo;; *)

(* Error: This expression has type < input : string -> 'a; .. > -> 'a
 *        but an expression was expected of type < input : int -> 'b; .. > -> 'b
 *               Types for method input are incompatible *)

type t = < m: int; ..>;;

(* Error: A type variable is unbound in this type declaration.
 * In type < m : int; .. > as 'a the variable 'a is unbound  *)
(* < m : int; .. > の別名である 'a が左辺で宣言されていない、というエラー *)

