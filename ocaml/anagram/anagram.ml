open Core_kernel.Std

module CM = Char.Map

let letter_count word =
    let counter acc char =
        let key = Char.lowercase char in
        let data = (CM.find acc key |> Option.value ~default:0) + 1
        in CM.add acc ~key ~data
    in
    String.lowercase word |>
    String.fold ~init:CM.empty ~f:counter

let anagrams word candidates =
    let base = letter_count word in
    let is_anagram candidate =
        (String.lowercase word <> String.lowercase candidate)
        && CM.equal (=) base (letter_count candidate)
    in
    List.filter candidates ~f:is_anagram
