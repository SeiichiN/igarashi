(* 12.5 より高度なクラス機能・型 *)
(* 12.5.1 抽象メソッドと抽象クラス *)

class demo_calc n m =
object (self)
  (* インスタンス変数 *)
  val mutable num = 0
  val mutable func = fun x -> x

  (* メソッド定義 *)
  method input n = num <- n
  method plus =
    let x = num in
    func <- (fun y -> x + y)
  method eq = func num

  initializer
    self#input n; self#plus; self#input m;
    Printf.printf "%d + %d = %d\n" n m (self#eq)
end;;

new demo_calc 32 28;;

class virtual abstract_calc_demo n m op_name =
  object (s)
    val mutable num = 0
    val mutable func = fun x -> x

    method input n = num <- n
    method virtual op : unit    (* 抽象メソッド *)
    method eq = func num
    initializer
      s#input n; s#op; s#input m;
      Printf.printf "%d %s %d = %d\n" n op_name m s#eq
  end;;

(*
 * class virtual abstract_calc_demo :
 *   int ->
 *   int ->
 *   string ->
 *   object
 *     val mutable func : int -> int
 *     val mutable num : int
 *     method eq : int
 *     method input : int -> unit
 *     method virtual op : unit
 *  end *)

(* new abstract_calc_demo 3 6 "op";; *)
(* Error: Cannot instantiate the virtual class abstract_calc_demo *)

