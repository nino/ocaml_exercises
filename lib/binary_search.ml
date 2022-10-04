module Slice = struct
  type 'a t = { array : 'a array; _start : int; _end : int }

  let length slice = slice._end - slice._start
  let first slice = slice.array.(slice._start)
  let of_array array = { array; _start = 0; _end = Array.length array }

  let first_half { array; _start; _end } =
    { array; _start; _end = (_start + _end) / 2 }

  let second_half { array; _start; _end } =
    { array; _start = (_start + _end) / 2; _end }
end

let middle_index array = Array.length array / 2

let find array value =
  let rec find' slice value =
    if Slice.length slice = 1 then
      if Slice.first slice = value then Ok slice._start
      else Error "value not in array"
    else if Slice.length slice = 0 then Error "value not in array"
    else
      match find' (Slice.first_half slice) value with
      | Ok index -> Ok index
      | Error _ -> (
          match find' (Slice.second_half slice) value with
          | Ok index -> Ok index
          | Error err -> Error err)
  in
  find' (Slice.of_array array) value
