open Core_kernel.Std

type base = int

let div_and_rem a b = a / b, a mod b

let from_base digits b =
    List.fold digits ~f:(fun acc d -> acc * b + d) ~init:0

let to_base value new_base =
    let rec aux n b digits =
        match n with
        | 0 -> digits
        | _ ->
            let div, rem = div_and_rem n b in
            aux div b (rem::digits)
    in
    aux value new_base []
    |> List.rev


let convert_bases ~from ~digits ~target =
    if from < 2 || target < 2 then None
    else if List.exists digits ~f:(fun digit -> digit >= from || digit < 0) then None
    else if from = target then Some digits
    else
        from_base digits from
        |> to_base target
        |> Option.some