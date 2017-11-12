open Core.Std

let to_roman n =
    let rec aux l n =
        if n >= 1000 then aux ('M'::l) (n-1000)
        else if n >= 900 then aux ('M'::'C'::l) (n-900)
        else if n >= 500 then aux ('D'::l) (n-500)
        else if n >= 400 then aux ('D'::'C'::l) (n-400)
        else if n >= 100 then aux ('C'::l) (n-100)
        else if n >= 90 then aux ('C'::'X'::l) (n-90)
        else if n >= 50 then aux ('L'::l) (n-50)
        else if n >= 40 then aux ('L'::'X'::l) (n-40)
        else if n >= 10 then aux ('X'::l) (n-10)
        else if n >= 9 then aux ('X'::'I'::l) (n-9)
        else if n >= 5 then aux ('V'::l) (n-5)
        else if n >= 4 then aux ('V'::'I'::l) (n-4)
        else if n >= 1 then aux ('I'::l) (n-1)
        else l
    in
    aux [] n
    |> List.rev
    |> String.of_char_list