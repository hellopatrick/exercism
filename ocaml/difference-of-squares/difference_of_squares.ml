(* Square the sum of the numbers up to the given number *)
let square_of_sum n =
    let sum = (n * (n + 1)) / 2 in
    sum * sum

(* Sum the squares of the numbers up to the given number *)
let sum_of_squares n = (n * (n + 1) * (2 * n + 1)) / 6

(* Subtract sum of squares from square of sums *)
let difference_of_squares n =
    square_of_sum n - sum_of_squares n