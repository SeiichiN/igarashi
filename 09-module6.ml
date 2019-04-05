(* 9.3.4 既存の型の隠蔽とシグネチャマッチング *)

module TableAL = (* AL = association list *)
    struct
        type ('a, 'b)t = ('a * 'b) list

        let empty = []

        let add key datum table = (key, datum) :: table

        let retrieve key table =
            try
                Some (List.assoc key table)
            with Not_found -> None

        let delete key table =
            List.filter (fun (key', datum) -> key <> key') table

        let rec dump = function
            [] -> []
            | (key, datum) :: rest ->
                    (key, datum) :: (dump (delete key rest))
    end;;

                
module type TABLE2 =
    sig
        type ('a, 'b) t (* = Empty | Entry of 'a * 'b * ('a, 'b) t *)

        val empty : ('a, 'b) t

        val add : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

        val retrieve : 'a -> ('a, 'b) t -> 'b option

        val dump : ('a, 'b) t -> ('a * 'b) list
    end;;

module AbsTableAL : TABLE2 = TableAL;;

