open Core.Std


let rec binary_search array k low high =
    if high < low then None
    else
        let mid = (high + low) / 2 in
        let mid_value = array.(mid) in
        if mid_value > k then binary_search array k low (mid - 1)
        else if mid_value < k then binary_search array k (mid + 1) high
        else Some mid

let find array k =
    if Array.is_empty array then None
    else binary_search array k 0 (Array.length array - 1)