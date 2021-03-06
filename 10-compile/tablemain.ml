let ( <<< ) table (key, content) = Table.add key content table

(* テーブルを作る *)
let table = Table.empty
<<< ("a", "the first letter of the English alphabet")
<<< ("b", "the second letter of the English alphabet")
<<< ("zzz", "sleeping noise")

(* a で検索した結果を表示 *)
let () =
    match Table.retrieve "a" table with
    Some x -> print_string x; print_newline ()
    | None -> ()

(* エントリの上書き *)
let table' = table <<< ("a", "an indefinite article")

(* 再び a で検索した結果を表示 *)
let () =
    match Table.retrieve "a" table' with
    Some x -> print_string x; print_newline ()
    | None -> ()

(* テーブルの内容の表示 *)
let () =
    List.iter
    (fun (key, body) ->
        print_string key;
        print_string ": ";
        print_string body;
        print_newline ())
    (Table.dump table')

