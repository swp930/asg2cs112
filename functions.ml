let increment = fun i -> i + 1;;
increment 2;; (* Prints out 3 *)

let sum = fun i j -> i + j;;
sum 1 2;; (* Prints out 3 *)

let sum i =
  let sum2 j =
    i+j
  in
    sum2;;

let rec power i x = if i = 0 then
    1.0
  else
    x *. (power(i - 1) x);;

let f ~x:i ~y:j = i - j;;

let g ?(x=1) y = x - y;;
g 1;;
g ~x:3 4;;
