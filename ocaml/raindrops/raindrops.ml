open Core_kernel

let raindrop n =
    let noises = [
        (3, "Pling");
        (5, "Plang");
        (7, "Plong");
    ] in
    let is_multiple k = n mod k = 0 in
    let f acc (k, noise) = if is_multiple k then acc ^ noise else acc in
    let sound = List.fold_left ~init:"" ~f:f noises in
    match sound with
    | "" -> string_of_int n
    | _ -> sound