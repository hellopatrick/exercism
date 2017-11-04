let leap_year year =
    let is_multiple n = year mod n = 0 in
    is_multiple 4 && (not (is_multiple 100) || is_multiple 400)