type planet = Mercury | Venus
            | Earth | Mars
            | Jupiter | Saturn
            | Neptune | Uranus

let earth_year = 31557600.0

let age_on planet age_in_seconds =
    let year_length = match planet with
    | Mercury -> 0.2408467 *. earth_year
    | Venus -> 0.61519726 *. earth_year
    | Earth -> 1.0 *. earth_year
    | Mars -> 1.8808158 *. earth_year
    | Jupiter -> 11.862615 *. earth_year
    | Saturn -> 29.447498 *. earth_year
    | Uranus -> 84.016846 *. earth_year
    | Neptune -> 164.79132 *. earth_year
    in
    float_of_int age_in_seconds /. year_length

