open Core

module CS = Char.Set

let tracker acc char =
    match Char.is_alpha char with
    | true -> CS.add acc char
    | false -> acc

let is_pangram sentence =
    let number_of_letters = String.lowercase sentence
    |> String.fold ~init:CS.empty ~f:tracker
    |> CS.length
    in number_of_letters = 26