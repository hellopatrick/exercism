open Core_kernel

type nucleotide = A | C | G | T

let hamming_distance first second =
    let zipped = List.zip first second in
    let folder acc (a, b) = if a = b then acc else acc + 1 in
    match zipped with
    | None -> None
    | Some l -> Some(List.fold_left ~init:0 ~f:folder l)