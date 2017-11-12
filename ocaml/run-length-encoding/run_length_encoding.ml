open Core.Std

let build (count, char) =
    match count with
    | 1 -> sprintf "%c" char
    | n -> sprintf "%d%c" count char

let encode s =
    String.fold s ~init:[] ~f:(fun acc c ->
        match acc with
        | (count, last)::tl when last = c -> (count + 1, last)::tl
        | _ -> (1,c)::acc
    )
    |> List.rev_map ~f:build
    |> String.concat

let breakpoint a b =
    not (Char.is_digit a)

let as_tuple g =
    match g with
    | [] -> (0, '?')
    | [c] -> (1, c)
    | t ->
        let digits, char = List.split_while t ~f:Char.is_digit in
        let n = String.of_char_list digits |> Int.of_string in
        match char with
        | [c] -> (n, c)
        | _ -> failwith "there should only be one char."

let as_string (n, c) =
    let rec build l = function
        | 0 -> l
        | n -> build (c::l) (n-1)
    in
    build [] n |> String.of_char_list

let group g =
    match g with
    | [] -> ""
    | [c] -> String.of_char c
    | g -> as_tuple g |> as_string

let decode s =
    String.to_list s
    |> List.group ~break:breakpoint
    |> List.map ~f:group
    |> String.concat
