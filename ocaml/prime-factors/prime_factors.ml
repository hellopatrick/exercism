open Core.Std

let factors_of (n:int64) =
    let rec aux n p l =
        let open Int64 in
        if n = 1L then []
        else if n < p * p then n::l
        else if n % p = 0L then aux (n / p) p (p::l)
        else if p = 2L then aux n 3L l
        else aux n (p + 2L) l
    in
    aux n 2L [] |> List.rev