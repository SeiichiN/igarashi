(* 15.1 「銀行口座」を作ってみる *)

#directory "+labltk";;
#load "labltk.cma";;
open Tk;;

Label.create;;
(*
- : ?name:string ->
    ?anchor:anchor ->
    ?background:color ->
    ?bitmap:bitmap ->
    ?borderwidth:int ->
    ?cursor:cursor ->
    ?font:string ->
    ?foreground:color ->
    ?height:int ->
    ?highlightbackground:color ->
    ?highlightcolor:color ->
    ?highlightthickness:int ->
    ?image:[< image ] ->
    ?justify:justification ->
    ?padx:int ->
    ?pady:int ->
    ?relief:relief ->
    ?takefocus:bool ->
    ?text:string ->
    ?textvariable:Textvariable.textVariable ->
    ?textwidth:int ->
    ?underline:int ->
    ?width:int ->
    ?wraplength:int -> 'a Widget.widget -> Widget.label Widget.widget
= <fun>
*)
