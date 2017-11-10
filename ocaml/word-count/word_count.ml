open Core.Std

module SM = String.Map

let normalize c =
    match c with
    | 'A'..'Z' -> Char.lowercase c
    | 'a'..'z' | '0'..'9' | '\'' -> c
    | _ -> ' '

let fix_quotes word =
    if String.is_prefix word ~prefix:"'" && String.is_suffix word ~suffix:"'" then
        String.slice word 1 (String.length word - 1)
    else
        word

let counter map word =
    if word = "" then
        map
    else
        SM.update map word ~f:(fun count ->
            match count with
            | None -> 1
            | Some j -> j + 1
        )

let word_count sentence =
    sentence
    |> String.map ~f:normalize
    |> String.split ~on: ' '
    |> List.map ~f:fix_quotes
    |> List.fold ~init:SM.empty ~f:counter
