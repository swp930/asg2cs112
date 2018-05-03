(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    (* cmp 
    ** Compares list1 and list2. Negative if list1 is of
    ** smaller magnitude. 
    ** Positive if list1 is of greater magnitude
    ** Zero if equal
    *)
    let rec cmp list1 list2=
        if (List.length list1) > (List.length list2) 
            then 1
        else if (List.length list1) < (List.length list2)
            then -1
        else match (list1, list2) with
            | [],[]            -> 0
            | list1, []        -> 1
            | [], list2        -> -1
            | list1, list2     ->
                let revList1 = reverse list1 in
                let revList2 = reverse list2 in
                if (car revList1) > (car revList2)
                    then 1
                else if (car revList1) < (car revList2)
                    then -1
                else cmp (reverse (cdr revList1)) (reverse (cdr revList2))
                

    let trimzeros list =
        let rec trimzeros' list' = match list' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                 let cdr' = trimzeros' cdr
                 in  match car, cdr' with
                     | 0, [] -> []
                     | car, cdr' -> car::cdr'
        in trimzeros' list

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)
    
    let rec sub' list1 list2 borrow = match (list1, list2, borrow ) with
        | list1, [], 0          -> list1
        | [], list2, 0          -> []  (*should not happen*)
        | list1, [], borrow     ->sub' list1 [borrow] 0
        | [], list2, borrow     -> []  (*should not happen*)
        | car1::cdr1, car2::cdr2, borrow ->
            let difference= car1 - car2 - borrow in
            if difference>= 0
                then car1-car2-borrow::sub' (cdr list1) (cdr list2) 0
            else car1+radix-car2::sub' (cdr list1) (cdr list2) 1
                
    


    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
            then Bigint (neg1, add' value1 value2 0)
        else let compare = cmp value1 value2 in
            if compare > 0 then Bigint (neg1, trimzeros(sub' value1 value2 0))
            else if compare<0 then Bigint(neg2, trimzeros(sub' value2 value1 0))
            else zero 
            

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        match(neg1, neg2) with
            | Pos, Pos    ->
                let compare = cmp value1 value2 in
                if compare > 0 
                    then (Bigint (Pos, trimzeros(sub' value1 value2 0)))
                else if compare < 0
                    then (Bigint (Neg, trimzeros(sub' value2 value1 0)))
                else zero
            | Pos, Neg    -> (Bigint (Pos, add' value1 value2 0))
            | Neg, Pos    -> (Bigint (Neg, add' value1 value2 0))
            | Neg, Neg    ->
                let compare = cmp value1 value2 in 
                if compare > 0
                    then (Bigint (Neg, sub' value1 value2 0))
                else if compare < 0
                    then (Bigint (Pos, sub' value2 value1 0))
                else zero


        

    let mul = add

    let div = add

    let rem = add

    let pow = add

end

