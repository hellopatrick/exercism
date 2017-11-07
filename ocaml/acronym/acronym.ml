open Core

let delimiters = [' '; '-']

let acronym title =
    String.split_on_chars title ~on:delimiters
    |> List.map ~f:(fun s -> String.get s 0)
    |> String.of_char_list
    |> String.uppercase