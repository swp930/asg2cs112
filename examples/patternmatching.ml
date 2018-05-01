let rec fib i =
  match i with
    0 -> 0
  | 1 -> 1
  | j -> fib(j-2) + fib(j-1);;

let is_uppercase = function
  'A' .. 'Z' -> true
  | c -> false;;

let is_uppercase = function
  'A' .. 'Z' -> true
  | d -> false;;

let is_uppercase = function 'A' .. 'Z' -> true;;

let is_odd i =
  match i mod 2 with
      0 -> false
    | 1 -> true;;

let is_odd i =
  match i mod 2 with
      0 -> false
    | 1 -> true
    | _ -> raise (Invalid_argument "is_odd");;
