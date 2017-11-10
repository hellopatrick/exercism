open Core.Std

let bottles = function
    | 0 -> "no more bottles of beer"
    | 1 -> "1 bottle of beer"
    | n -> string_of_int n ^ " bottles of beer"

let noun = function
    | 0 -> "it"
    | _ -> "one"

let first_line n =
    String.concat [bottles n; "on the wall,"; bottles n ^ ".\n";] ~sep:" "

let second_line n =
    String.concat ["Take"; noun n; "down and pass it around,"; bottles n; "on the wall.\n";] ~sep:" "

let verse n =
    match n with
    | 0 -> String.capitalize (first_line n) ^ "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    | n -> first_line n ^ second_line (n-1)

let lyrics ~from ~until =
    List.range ~stride:(-1) ~start:`inclusive ~stop:`inclusive from until
    |> List.map ~f:verse
    |> String.concat ~sep:"\n"

