let p = 1, "Hello";;
let x, y = p;;

let fst (x, _) = x;;
let snd (_, y) = y;;
fst p;;
snd p;;

let p3 = 1, "Hello", 4;;
let fst3 (x, _, _) = x;;
let snd3 (_, _, y) = y;;

let x = 1;;
let y = 2;;
let x,y = y,x;;
(*Swaps the values of x and y*)

(*Not sure how the following works
let make_coord x y = x, y;;
let x_of_coord = fst;;
let y_of_coord = snd;;
*)
