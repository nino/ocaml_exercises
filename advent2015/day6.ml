module Instruction = struct
  type action = Switch_on | Switch_off | Toggle
  type rect = { from_corner : int * int; to_corner : int * int }
  type t = action * rect

  let create action from_x from_y to_x to_y =
    (action, { from_corner = (from_x, from_y); to_corner = (to_x, to_y) })

  let t_of_string string =
    let pattern =
      Re.Pcre.re
        {|^\s*(turn on|turn off|toggle) +(\d+),(\d+) +through +(\d+),(\d+)\s*$|}
      |> Re.compile
    in
    match Re.Pcre.exec ~rex:pattern string |> Re.Group.all with
    | [| _; action; from_x; from_y; to_x; to_y |] ->
        (* The matches from the regex should only contain numbers, so none of
           these conversions should never raise an exception *)
        let action =
          match action with
          | "turn on" -> Switch_on
          | "turn off" -> Switch_off
          | "toggle" -> Toggle
          | _ -> failwith "reached unreachable code"
        in
        let from_x = Int.of_string from_x in
        let from_y = Int.of_string from_y in
        let to_x = Int.of_string to_x in
        let to_y = Int.of_string to_y in
        Ok (create action from_x from_y to_x to_y)
    | _ -> Error (Error.of_string "Unable to parse instruction")

  let t_list_of_string_list strlist =
    List.map strlist ~f:t_of_string |> Result.all
end

module Grid = struct
  type t = { size : int; mutable lamps : bool array }

  let create size = { size; lamps = Array.create ~len:(size * size) false }
  let coord grid x y = x + (grid.size * y)

  let update grid rect update_fun =
    let to_x, to_y = rect.Instruction.to_corner in
    let from_x, from_y = rect.Instruction.from_corner in
    let x = ref from_x in
    let y = ref from_y in
    while !y <= to_y do
      while !x <= to_x do
        let oldvalue = grid.lamps.(coord grid !x !y) in
        let newvalue = update_fun oldvalue in
        grid.lamps.(coord grid !x !y) <- newvalue;
        Int.incr x
      done;
      x := from_x;
      Int.incr y
    done

  let run grid (action, rect) =
    match action with
    | Instruction.Switch_on -> update grid rect (fun _ -> true)
    | Instruction.Switch_off -> update grid rect (fun _ -> false)
    | Instruction.Toggle -> update grid rect (fun x -> not x)

  let count_lit grid = Array.count grid.lamps ~f:(fun x -> x)
end

let execute raw_instructions =
  match Instruction.t_list_of_string_list raw_instructions with
  | Error err -> Error err
  | Ok parsed_instructions ->
      let grid = Grid.create 1000 in
      List.iter parsed_instructions ~f:(fun instruction ->
          Grid.run grid instruction);
      Ok grid

let tests () =
  let grid = Grid.create 1000 in
  assert (Grid.count_lit grid = 0);
  Grid.run grid
    (Instruction.Switch_on, { from_corner = (0, 0); to_corner = (2, 2) });
  assert (Grid.count_lit grid = 9)

let run () =
  Utils.greet 6;

  let input_path = Utils.input_path_for_day 6 in
  tests ();
  match Stdio.In_channel.read_lines input_path with
  | exception ex ->
      Stdio.printf "Error reading input: %s\n%!" (Exn.to_string ex)
  | raw_instructions -> (
      match execute raw_instructions with
      | Ok finished_grid ->
          Stdio.printf "%d lights are lit.\n" (Grid.count_lit finished_grid)
      | Error err ->
          Stdio.printf "Unable to run program: %s\n" (Error.to_string_hum err))
