let x = 1;;
let y = 2;;
let z = x + y;;
z;;
(* z = 3 *)

let z =
  let x = 1 in
  let y = 2 in
    x + y ;;
(* z = 3 *)

let x = 7 in let y =
  let x = 2 in x+1
    in
    x + y;;
(* 10 *)
