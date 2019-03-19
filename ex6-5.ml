(* 練習問題 6.5 *)

(*
深さ n ですべてのノードのラベルが x であるような完全二分木を生成する関数 comptree x n を定義しなさい。次に、ノードのラベルとして、根が 1 で、次の子は右から 2、3、次の深さには 4、5、6、7 と、各深さに連続した数字が並んでいるような完全二分木を生成する関数 comptree' n を定義しなさい。
 *)

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec comptree x n =
  if n = 0 then Lf
  else Br (x, (comptree x (n-1)), (comptree x (n-1)));;
  
         
(*
comptree の例  
 Br(x, Br(x, Br(x, Lf, Lf), Br(x, Lf, Lf)),
                  Br(x, Br(x, Lf, Lf), Br(x, Lf, Lf)));;
 *)
let joh x n =
    let rec joh_in x n a =
        if n = 0 then a
    else joh_in x (n-1) (x * a)
    in
    joh_in x n 1;;

          
let comptree' n =
  let rec comptree_in x n =
    if n = 0 then Lf
    else Br ((joh 2 (n-1)), (comptree_in ((joh 2 n) + 1) (n-1)), (comptree_in (joh 2 n) (n-1)))
  in 
  comptree_in 1 n;;
  
