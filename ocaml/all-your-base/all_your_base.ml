open Core_kernel.Std

type base = int

let from_base digits b =
    List.fold digits ~f:(fun acc d -> acc * b + d) ~init:0

let to_base value new_base =
    let rec aux n b digits =
        match n with
        | 0 -> digits
        | _ -> aux (n / b) b ((n mod b)::digits)
    in
    aux value new_base []


let convert_bases ~from ~digits ~target =
    if from < 2 || target < 2 then None
    else if List.exists digits ~f:(fun digit -> digit >= from || digit < 0) then None
    else if digits = [] then None
    else
        match from_base digits from with
        | 0 -> Some [0]
        | k -> Some (to_base k target)