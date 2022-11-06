let assets_path () =
  let cwd = Unix.getcwd () in
  Caml.Filename.concat cwd "advent2015/assets"

let input_path_for_day day =
  let day = Int.to_string day in
  Caml.Filename.concat (assets_path ()) ("day" ^ day ^ "input.txt")
