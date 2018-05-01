let l = "Hello" :: "World" :: [];;

let rec sum = function
    [] -> 0
  |i :: l -> i + sum l;;

sum [1; 2; 3; 4];;

let rec sumString = function
    [] -> ""
  |w :: a -> w ^ sumString a;;
sumString["a"; "b"; "c"]

let rec mem x l =
  match l with
    [] -> false
  | y :: l -> x = y || mem x l;;

let l1 = [1; 2; 3];;
let l2 = [1; 2; 3; 4];;

mem 4 l1;; (* Returns false *)
mem 4 l2;; (* Returns true *)

let rec map f = function
    [] -> []
  | x :: l -> f x :: map f l;;

map (fun i -> (float_of_int i) +. 0.5) [1; 2; 3; 4];;

let entry = [("name", "Jason"); ("height", "6 3")];;

(* Non-tail recursive *)
let rec fact1 i =
  if i = 0 then
    1
  else
   i * fact1(i - 1);;

(* Tail recursive *)
let fact2 i =
  let rec loop accum i =
    if i = 0 then
      accum
    else
      loop (i * accum) (i-1)
  in
   loop 1 i;;
