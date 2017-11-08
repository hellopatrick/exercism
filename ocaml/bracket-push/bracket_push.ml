open Core_kernel.Std

let append stack char =
    match char, stack with
    | ')', '('::xs -> xs
    | '}', '{'::xs -> xs
    | ']', '['::xs -> xs
    | _, _ -> char :: stack


let are_balanced str =
    String.fold str ~init:[] ~f:(
        fun accum char ->
        match char with
        | '{' | '}' | '(' | ')' | '[' | ']' -> append accum char
        | _ -> accum
    ) |> List.is_empty