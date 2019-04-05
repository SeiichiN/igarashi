(* 9.2 モジュール定義 *)

module Tree =
    struct
        type 'a t = Lf | Br of 'a * 'a t * 'a t

        let rec size = function
            Lf -> 0
            | Br (_, left, right) -> 1 + size left + size right

        let rec depth = function
            Lf -> 0
            | Br (_, left, right) -> 1 + max (depth left) (depth right)
    end;;

let tr = Tree.Br (1, Tree.Lf, Tree.Br(2, Tree.Lf, Tree.Lf));;

Tree.depth tr;;

(* ドット記法とレコードアクセス *)

module M =
    struct
        type r = {a : int; b : int}
    end;;

let x = {M.a = 1; M.b = 2};;

x.M.a + x.M.b;;

(*
モジュール内でレコード型の要素を定義すると、その要素を取り出すのにも「.」を使うから、モジュールを指定する「.」もあるので、ちょっとややこしい。
 *)
