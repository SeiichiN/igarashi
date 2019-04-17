(* オブジェクト指向 *)

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
  
(*
class calc :
  object
    val mutable func : int -> int
    val mutable num : int
    method eq : int
    method input : int -> unit
    method plus : unit
  end
*)

let c = new calc;;
  (* val c : calc = <obj> *)
  
c#input 4; c#plus; c#input 2; c#eq;;
    (* - : int = 6 *)

type base =
    Oct  (* 8進数 *)
  | Dec  (* 10進数 *)
  | Hex  (* 16進数 *);;

  (* type base = Oct | Dec | Hex *)

let string_of = function
    Oct -> Printf.sprintf "%o"
  | Dec -> Printf.sprintf "%d"
  | Hex -> Printf.sprintf "%x";;

  (* val string_of : base -> int -> string = <fun> *)

class calc_base b =
object
  val mutable num = 0
  val mutable func = fun x -> x

  method input n = num <- n
  method plus =
    let x = num in
    func <- (fun y -> x + y)
  method eq = string_of b (func num)
end;;
  
(*
class calc_base :
  base ->
  object
    val mutable func : int -> int
    val mutable num : int
    method eq : string
    method input : int -> unit
    method plus : unit
  end
 *)

let c = new calc_base Hex;;
  
c#input 17; c#plus; c#input 31; c#eq;;


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

let d = new calc_double;;
  
d#input 12; d#double;;
  
d#input 4; d#double;;
    
(* 12.2.1 補助的メソッドと private メソッド *)

class calc_many_buttons =
object (self)
  val mutable num = 0
  val mutable func = fun x -> x

  (* 数字入力メソッドから使われる補助メソッド shift *)
  method shift n = num <- num * 10 + n  (* 0 <= n <= 9 *)

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
  
(*
class calc_many_buttons :
  object
    val mutable func : int -> int
    val mutable num : int
    method eight : unit
    method eq : int
    method five : unit
    method four : unit
    method nine : unit
    method one : unit
    method plus : unit
    method seven : unit
    method shift : int -> unit
    method six : unit
    method three : unit
    method two : unit
    method zero : unit
  end
*)

let c = new calc_many_buttons;;

  (* val c : calc_many_buttons = <obj> *)
  
c#one; c#zero; c#plus; c#two; c#nine; c#eq;;  (* 10 + 29 = ? *)

  (* - : int = 39 *)

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

let c = new calc_many_buttons';;

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

  (* 32 + 28 = 60 *)

  (* - : demo_calc = <obj>   *)

  
