let person1 = ("Sato", 170.5, 68.0)
let person2 = ("Suzuki", 156.5, 75.5)
let person3 = ("Kimura", 180.0, 78.0)
let nukayama = ("Seiichi", 170.0, 68.5)

let (name, height, weight) = person1;;
let (name, height, weight) = person2;;
let (name, height, weight) = person3;;
let (name, height, weight) = nukayama;;

(*
  bmi: String, float, float => String
 *)
let bmi (name, height, weight) =
  let bmiLevel = weight /. ((height /. 100.) *. (height /. 100.)) in
  if (bmiLevel < 18.5)
  then name ^ "さんはやせています。"
  else
    if (bmiLevel >= 25. && bmiLevel < 30.)
    then name ^ "さんは肥満です。"
    else
      if (bmiLevel >= 30.)
      then name ^ "さんは高度肥満です。"
      else
        name ^ "さんは標準です。"



