open Core.Std

let check pn =
    match pn.[0], pn.[3] with
    | '0'..'1', _ | _, '0'..'1' -> None
    | _, _ -> Some pn

let number pn =
    let just_digits = String.filter pn ~f:Char.is_digit in
    match String.length just_digits with
    | 10 -> check just_digits
    | 11 -> if just_digits.[0] = '1' then check (String.drop_prefix just_digits 1) else None
    | _ -> None