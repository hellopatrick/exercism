let transform old_list =
    let expand (point, letters) = List.map (fun letter -> (Char.lowercase_ascii letter, point)) letters in
    let expanded = List.map expand old_list in
    let flattened = List.flatten expanded in
    List.sort compare flattened
