type planet = Mercury | Venus
            | Earth | Mars
            | Jupiter | Saturn
            | Neptune | Uranus

let age_on planet age_in_seconds =
    let multiplier = match planet with
    | Mercury -> 0.2408467 *. 31557600.0
    | Venus -> 0.61519726 *. 31557600.0
    | Earth -> 31557600.0
    | Mars -> 1.8808158 *. 31557600.0
    | Jupiter -> 11.862615 *. 31557600.0
    | Saturn -> 29.447498 *. 31557600.0
    | Uranus -> 84.016846 *. 31557600.0
    | Neptune -> 164.79132 *. 31557600.0
    in
    float_of_int age_in_seconds /. multiplier

