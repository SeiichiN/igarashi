(* downto1 *)

let rec downto1 n =
  if n = 1 then [1]
  else
    n :: downto1 (n-1)

let rec print_list = function
    [] -> ()
    | e :: rest ->
            print_int e; print_string " "; print_list rest

let () = print_list (downto1 10); print_newline ()

