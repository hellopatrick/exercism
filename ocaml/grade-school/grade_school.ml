open Core

module IM = Int.Map

type school = string list IM.t

let create () = IM.empty

let add name grade school = IM.add_multi school ~key:grade name

let grade grade school =
    match IM.find school grade with
    | Some students -> students
    | None -> []

let sort = IM.map ~f:(List.sort ~cmp:compare)

let to_map school = school