let assets_path () =
  let cwd = Unix.getcwd () in
  Filename.concat cwd "advent2022/assets"

let input_path_for_day day =
  let day = Int.to_string day in
  Filename.concat (assets_path ()) ("day" ^ day ^ ".txt")

let extra_path filename = Filename.concat (assets_path ()) filename
