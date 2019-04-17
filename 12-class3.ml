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
(* 抽象クラスはインスタンス作成できない。   *)

(* 具体的クラスを作成する。その中で、抽象クラスを継承する。 *)
class calc_demo_plus n m =
  object
    inherit abstract_calc_demo n m "+"
    method op = let x = num in func <- (fun y -> x + y)
  end;;

new calc_demo_plus 6 9;;
(* 6 + 9 = 15
 * - : calc_demo_plus = <obj> *)


class calc_demo_mult n m =
  object
    inherit abstract_calc_demo n m "*"
    method op = let x = num in func <- (fun y -> x * y)
  end;;

new calc_demo_mult 6 9;;
(* 6 * 9 = 54
 * - : calc_demo_mult = <obj>  *)

(************************************************************************)
(* 12.5.2 private めそっどの継承、クラス型注釈によるメソッド隠蔽 *)

class calc_many_buttons' =
object (self)
  val mutable num = 0
  val mutable func = fun x -> x

  (* 数字入力メソッドから使われる補助メソッド shift *)
  method private shift n = num <- num * 10 + n  (* 0 <= n <= 9 *)

  method zero = self#shift 0
  method one = self#shift 1
  method two = self#shift 2
  method three = self#shift 3
  method four = self#shift 4
  method five = self#shift 5
  method six = self#shift 6
  method seven = self#shift 7
  method eight = self#shift 8
  method nine = self#shift 9

  method plus =
    let x = num in
    func <- (fun y -> x + y);
    num <- 0  (* num を 0 クリアする *)

  method eq =
    let r = func num in
    num <- 0;  (* num と func をクリアする *)
    func <- (fun n -> n);
    r

  method du = num
end;;

(* 継承もとの private メソッドを呼び出すことができる *)
class calc_many_buttons_00 =
object
  inherit calc_many_buttons' as super
  method zerozero = super#shift 0; super#shift 0
end;;

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
  
  
class calc_counter =
object (s)
  inherit calc as super
  val mutable c = 0

  method get = c
  method private incr = c <- c + 1
  method eq = s#incr; super#eq  (* 定義の上書き *)
end;;

(*
class calc_counter :
  object
    val mutable c : int
    val mutable func : int -> int
    val mutable num : int
    method eq : int
    method get : int
    method private incr : unit
    method input : int -> unit
    method plus : unit
  end
*)  

class calc_counter :  (* 明示的なクラス型宣言 *)
  object
    inherit calc
    method get : int
  end
  =
object (s)
  inherit calc as super
  val mutable c = 0   (* コンパイラの応答では c は隠されている *)

  method get = c
  method private incr = c <- c + 1
  method eq = s#incr; super#eq
end;;
(*
class calc_counter :
  object
    val mutable func : int -> int
    val mutable num : int
    method eq : int
    method get : int
    method input : int -> unit
    method plus : unit
  end

calc クラスのインスタンス変数は現れているが、
calc_counter クラスで定義されたインスタンス変数 c は表示されない
*)  

class virtual foo :
  object
    val a : int
    method virtual m : int -> int
  end
  =
  object
    val mutable a = 0
    method m x = 100 + x
  end;;
(*
class virtual foo :
  object
    val a : int 
    method virtual m : int -> int
  end

インスタンス変数が mutable であることを隠す。
メソッドの存在を隠して、抽象メソッドとして外部に見せる
 *)

  (* クラス型も名前をつけて宣言できる *)
class type calc_counter_t =
    object
      inherit calc
      method get : int
    end;;

class calc_counter : calc_counter_t =
object (s)
  inherit calc as super
  val mutable c = 0

  method get = c
  method private incr = c <- c + 1
  method eq = s#incr; super#eq
end;;
  
