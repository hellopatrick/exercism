open Core

module CM = Char.Map

(* Count the number of times the nucleotide occurs in the string. *)
let count str c =
    String.count str ~f:(fun chr -> chr = c)

(* Count the nucleotides in the string. *)
let nucleotide_counts str =
    let cm = CM.of_alist_exn [
        ('A', count str 'A');
        ('T', count str 'T');
        ('C', count str 'C');
        ('G', count str 'G');
    ] in
    CM.filter cm ~f:(fun value -> value <> 0)