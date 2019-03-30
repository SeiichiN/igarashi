(* 8.3 制御構造 *)

let () = print_string "Hello, " in
print_string "World!\n";;

let f x y = 1 in
f (print_string "Hello, ") (print_string "World\n");;

(print_string "Hello, ", print_string "World\n");;

