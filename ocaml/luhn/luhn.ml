open Core.Std

let double_digit n =
    let d = n * 2 in
    if d >= 10 then d - 9 else d

let valid s =
    let ccn = String.filter s ~f:(fun c -> not (Char.is_whitespace c)) in
    if String.length ccn >= 2 then
        let sum = String.to_list ccn
        |> List.rev_map ~f:(fun c -> Char.to_int c - Char.to_int '0')
        |> List.rev_mapi ~f:(fun i d -> if i % 2 <> 0 then double_digit d else d)
        |> List.fold ~init:0 ~f:(+) in
        sum mod 10 = 0
    else false