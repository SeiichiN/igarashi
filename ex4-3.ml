(* 練習問題 4.3 *)

let id x = x;;

let ($) f g x = f (g x);;

(*
 * (f$f)x = f(fx) と同じことになる。
 * すなわち、n が偶数の場合、(f$f)となる。
 * (n/2) がゼロになるまで、(f$f)(f$f)...となる。
 * n が奇数の場合、(f$f)(f$f)...の最後に $f として、
 * f がひとつ追加される。
 * id とは、恒等関数である。すなわり、値なら値を、
 * 関数なら関数をそのまま返す関数である。
 *
 * 要するに、funny 関数は、n の回数だけ、f 関数を
 * 繰り返す関数を返す関数である。
 *)
let rec funny f n =
    if n = 0 then id
    else
        if n mod 2 = 0 then funny (f $ f) (n / 2)
        else
            funny (f $ f) (n / 2) $ f;;

let twice x = x * 2;;

let test1 = twice 2 = 4;;

let test2 = funny twice 4 2 = 32;;

let rec aaa f n =
    if n = 0 then id
    else
        if n mod 2 = 0
        then aaa (f $ f) (n/2)
        else
            aaa (f $ f) (n/2) $ f;;

let bbb f n = (f $ f) (n / 2);;

(* let ccc f n = (f $ f) (n) $ f;; i*)

(* let ddd f n = (f $ f) (n / 2) $ f;; *)

let addten x = x + 10;;

