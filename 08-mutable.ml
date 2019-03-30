(* 書き換え可能なデータ構造 *)

let s = "life";;
s.[2] <- 'k';;
s;;

let pair = ("life", 2);;
(fst pair).[2] <- 'k';;
pair;;

let pair = ("life", "life");;
(fst pair).[2] <- 'k';;
pair;;

let pair = let p = "life" in (p, p);;
(fst pair).[2] <- 'k';;
pair;;
          
let s = "life";;
let pair1 = ("life", s);;
let pair2 = (s, s);;

(pair1 = pair2, fst pair1 = fst pair2, snd pair1 = snd pair2);;
(pair1 == pair2, fst pair1 == fst pair2, snd pair1 == snd pair2);;

let update_string s1 s2 =
    let () = s1.[0] <- 'a' in
    let () = s2.[0] <- 'b' in
    s1.[0] = s2.[0];;

update_string "xyz" "xyz";;

let s = "xyz" in update_string s s;;

type teacher = {name : string; mutable office : string};;

let t = {name = "Igarashi"; office = "140"};;

t.office <- "142";;
t;;

(* ref -- 参照 （5という値が格納されているメモリへの参照） *)
let p = ref 5 and q = ref 2;;
(* ! -- 参照先から格納された値をとりだす *)
(!p, !q);;
(* := -- 参照先の値の書き換え *)
p := !p + !q;;

let reflist = [p; q; p];;
p := 100;;
reflist;;

(* 参照の参照 *)
let p = ref 5 and q = ref 2;;
let refp = ref p and refq = ref q;;
!refq := !(!refp);;
(!p, !q);;

(* 配列 *)
let ar = [| 1; 2 |];;
let [| b; c |] = ar;;

(* 多相性 *)
let x = ref [];;
let y = ref [];;
(true :: !y);; (* <= この時点で、bool list となる *)
(2 :: !x);;    (* <= この時点で、int list となる *)
x := [1];;
x;;

let (get, set) =
    let r = ref [] in
    ((fun () -> !r), (fun x -> r := x));;

(* 1 :: get ();; *)
"abc" :: get ();;
set ["abc"];;
1 :: get () ;;
