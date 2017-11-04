(** Rna-transcription exercise *)

type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let to_rna dna_list =
    let convert = function
    | `G -> `C
    | `C -> `G
    | `T -> `A
    | `A -> `U
    in
    List.map convert dna_list
