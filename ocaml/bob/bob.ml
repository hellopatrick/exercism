let is_quiet s = s = ""

let is_question s =
    let length = String.length s in
    match String.rindex_opt s '?' with
    | Some i -> i = length - 1
    | None -> false

let is_loud s = String.uppercase_ascii s = s && String.lowercase_ascii s <> s

let response_for s =
    let statement = String.trim s in
    if is_quiet statement then "Fine. Be that way!"
    else if is_loud statement then "Whoa, chill out!"
    else if is_question statement then "Sure."
    else "Whatever."