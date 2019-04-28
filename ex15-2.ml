(* プログラミング in OCaml 練習問題 15.2 *)

(* 練習問題 15.2 *)

(*
銀行口座プログラムを改造して、預金、引き出しの履歴を記録してリストボックスに表示しなさい。
 *)
#directory "+labltk";;
#load "labltk.cma";;

open Tk;;

let balance = ref 0;;  (* 残高 *)

let add_balance x = balance := !balance + x;;  (* 残高を更新する *)

let top = openTk();;  (* アプリ全体のウィンドウ *)

let tv_balance = Textvariable.create ();;  (* ラベルに表示する文字 *)

let label1 = Label.create top ~textvariable:tv_balance ~relief: `Raised;;

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

let bot_frame = Frame.create top;;


let entry = Entry.create bot_frame
and label2 = Label.create bot_frame ~text:"円"
and rb_frame = Frame.create bot_frame;;

let tv_button = Textvariable.create ();;


let radiobuttons = 
    List.map
    (fun (t, a) ->
        Radiobutton.create rb_frame ~text:t ~value:a ~variable:tv_button)
    [("を預金する", "Deposit");
    ("を引き出す", "Withdraw")];;

let list_frame = Frame.create top;;

let l_text = ref ""
and listbox = Listbox.create list_frame;;

let add_list yen =
  if yen < 0
  then
    l_text := Printf.sprintf "引出: %8d 円" yen
  else
    l_text := Printf.sprintf "預入: %8d 円" yen;
  Listbox.insert ~index:`End ~texts:[!l_text] listbox
  
let action entry tv_but tv_bal () =
    let y = int_of_string (Entry.get entry) in
    match Textvariable.get tv_but with
    "Deposit" -> add_balance y; print_balance tv_bal; add_list y
    | "Withdraw" -> add_balance (-y); print_balance tv_bal; add_list (-y)
    | _ -> failwith "Cannot happen";;
           

let button_ok = Button.create bot_frame
    ~text:"実行"
    ~command:(action entry tv_button tv_balance);;



let end_frame = Frame.create top;;
  
let end_action () = 
  (fun () -> closeTk(); exit 0);;
  
let button_end = Button.create end_frame
                               ~text:"終了"
                               ~command:(end_action ());;

let sb = Scrollbar.create list_frame;;

Listbox.configure ~yscrollcommand:(Scrollbar.set sb) listbox;;
Scrollbar.configure ~command:(Listbox.yview listbox)  sb;;


pack radiobuttons ~side:`Top;;

pack [coe entry; coe label2; coe rb_frame; coe button_ok] ~side:`Left;;

  pack [coe button_end] ~side:`Right;;

    pack [coe listbox; coe sb] ~side:`Left ~fill:`Y;;
  
pack [coe label1; coe bot_frame; coe list_frame; coe end_frame] ~side:`Top;;

print_balance tv_balance;;
