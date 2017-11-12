open Core.Std

let number_to_word = function
    | 1L -> "one"
    | 2L -> "two"
    | 3L -> "three"
    | 4L -> "four"
    | 5L -> "five"
    | 6L -> "six"
    | 7L -> "seven"
    | 8L -> "eight"
    | 9L -> "nine"
    | 10L -> "ten"
    | 11L -> "eleven"
    | 12L -> "twelve"
    | 13L -> "thirteen"
    | 14L -> "fourteen"
    | 15L -> "fifteen"
    | 16L -> "sixteen"
    | 17L -> "seventeen"
    | 18L -> "eighteen"
    | 19L -> "nineteen"
    | _ -> ""

let tens_to_word = function
    | 2L -> "twenty"
    | 3L -> "thirty"
    | 4L -> "forty"
    | 5L -> "fifty"
    | 6L -> "sixty"
    | 7L -> "seventy"
    | 8L -> "eighty"
    | 9L -> "ninety"
    | _ -> ""

let larger_number_to_word n =
    let open Int64 in
    let tens = n / 10L in
    let ones = n % 10L in
    let sep = if ones = 0L then "" else "-" in
    String.concat [tens_to_word tens; sep; number_to_word ones]

let to_word n =
    let rec aux n l =
        let open Int64 in
        if n >= 100L then aux (n % 100L) ("hundred"::(number_to_word (n / 100L))::l)
        else if n >= 20L then (larger_number_to_word n)::l
        else (number_to_word n)::l
    in aux n []
    |> List.rev
    |> String.concat ~sep:" "

let build n suffix =
    match n with
    | 0L -> None
    | n -> Some ((to_word n) ^ suffix)

let in_english n =
    if n < 0L || n > 999_999_999_999L then None
    else if n = 0L then Some "zero"
    else
        let open Int64 in
        let hundreds = n % 1000L in
        let thousands = (n / 1_000L) % 1000L in
        let millions = (n / 1_000_000L) % 1000L in
        let billions = (n / 1_000_000_000L) % 1000L in
        let parts = [
            build billions " billion";
            build millions " million";
            build thousands " thousand";
            build hundreds "";
        ] in
        List.filter_opt parts
        |> String.concat ~sep:" "
        |> String.strip
        |> Option.some