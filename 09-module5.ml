(* 9.3.3 型の定義内容を隠す：抽象データ型 *)

module Table =
    struct
        type ('a, 'b) t = Empty | Entry of 'a * 'b * ('a, 'b) t

        let empty = Empty

        let add key datum table = Entry (key, datum, table)

        let rec retrieve key = function
            Empty -> None
            | Entry (key', datum, rest) ->
                    if key = key' then Some datum
                    else retrieve key rest

        let rec delete key = function
            Empty -> Empty
            | Entry (key', datum, rest) ->
                    if key = key' then delete key rest
                    else Entry (key', datum, delete key rest)

        let rec dump = function
            Empty -> []
            | Entry (key, contents, rest) ->
                    (key, contents) :: (dump (delete key rest))
    end;;

module type TABLE2 =
    sig
        type ('a, 'b) t (* = Empty | Entry of 'a * 'b * ('a, 'b) t *)

        val empty : ('a, 'b) t

        val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

        val retrieve : 'a -> ('a, 'b) t -> 'b option

        val dump : ('a, 'b) t -> ('a * 'b) list
    end;;

module AbsTable : TABLE2 = Table;;


let atable = AbsTable.add "a" "the first letter of the English alphabet" AbsTable.empty;;
  (* val atable : (string, string) AbsTable.t = <abstr>   *)
  
