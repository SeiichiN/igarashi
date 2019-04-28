(* プログラミング in OCaml 練習問題 15.1 *)

(* 練習問題 15.1 *)

(*
銀行口座プログラムを改造して、残高がマイナスになった時に、（終了するのではなくて）残高表示の文字が赤になるようにしなさい。
 *)

#directory "+labltk";;
#load "labltk.cma";;

open Tk;;

let balance = ref 0;;  (* 残高 *)
(* val balance : int ref = {contents = 0} *)

let add_balance x = balance := !balance + x;;  (* 残高を更新する *)
(* val add_balance : int -> unit = <fun> *)

let top = openTk();;  (* アプリ全体のウィンドウ *)
(* val top : Widget.toplevel Widget.widget = <abstr> *)

let tv_balance = Textvariable.create ();;  (* ラベルに表示する文字 *)
(* val tv_balance : Textvariable.textVariable = <abstr> *)

let label1 = Label.create top ~textvariable:tv_balance ~relief: `Raised;;
(* val label1 : Widget.label Widget.widget = <abstr> *)

let print_balance tv =
  if !balance < 0
  then
    let s = Printf.sprintf "残高は%8d 円です" !balance in
    Textvariable.set tv s;
    Label.configure label1 ~foreground: `Red
  else
    let s = Printf.sprintf "残高は%8d 円です" !balance in
    Textvariable.set tv s;
    Label.configure label1 ~foreground: `Black;;
(* val print_balance : Textvariable.textVariable -> unit = <fun> *)

let bot_frame = Frame.create top;;
(* val bot_frame : Widget.frame Widget.widget = <abstr> *)

let entry = Entry.create bot_frame
and label2 = Label.create bot_frame ~text:"円"
and rb_frame = Frame.create bot_frame;;
(* val entry : Widget.entry Widget.widget = <abstr>
val label2 : Widget.label Widget.widget = <abstr>
val rb_frame : Widget.frame Widget.widget = <abstr> *)

let tv_button = Textvariable.create ();;
(* val tv_button : Textvariable.textVariable = <abstr> *)

let radiobuttons = 
    List.map
    (fun (t, a) ->
        Radiobutton.create rb_frame ~text:t ~value:a ~variable:tv_button)
    [("を預金する", "Deposit");
    ("を引き出す", "Withdraw")];;
(* val radiobuttons : Widget.radiobutton Widget.widget list = [<abstr>; <abstr>]
*)

let action entry tv_but tv_bal () =
    let y = int_of_string (Entry.get entry) in
    match Textvariable.get tv_but with
    "Deposit" -> add_balance y; print_balance tv_bal
    | "Withdraw" -> add_balance (-y); print_balance tv_bal
    | _ -> failwith "Cannot happen";;
(* val action :
  Widget.entry Widget.widget ->
  Textvariable.textVariable -> Textvariable.textVariable -> unit -> unit =
  <fun>  *)

let button_ok = Button.create bot_frame
    ~text:"実行"
    ~command:(action entry tv_button tv_balance);;
  (* val button : Widget.button Widget.widget = <abstr> *)


let end_frame = Frame.create top;;
  
let end_action () = 
  (fun () -> closeTk(); exit 0);;
  
let button_end = Button.create end_frame
                               ~text:"終了"
                               ~command:(end_action ());;
  

pack radiobuttons ~side:`Top;;

(* coe -- どんなwidgetでも any widget という同じ型に変換してくれる。 *)
pack [coe entry; coe label2; coe rb_frame; coe button_ok] ~side:`Left;;

pack [coe button_end] ~side:`Right;;
  
pack [coe label1; coe bot_frame; coe end_frame] ~side:`Top;;

print_balance tv_balance;;
