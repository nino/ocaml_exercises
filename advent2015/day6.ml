open Base

module Instruction = struct
  type action =
    | Switch_on
    | Switch_off
    | Toggle
    | Increase
    | Decrease
    | Increase_double

  type rect = { from_corner : int * int; to_corner : int * int }
  type interpretation_mode = Standard | Elvish

  let create action from_x from_y to_x to_y =
    (action, { from_corner = (from_x, from_y); to_corner = (to_x, to_y) })

  let t_of_string ~mode string =
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
          match (mode, action) with
          | Standard, "turn on" -> Switch_on
          | Standard, "turn off" -> Switch_off
          | Standard, "toggle" -> Toggle
          | Elvish, "turn on" -> Increase
          | Elvish, "turn off" -> Decrease
          | Elvish, "toggle" -> Increase_double
          | _ -> failwith "reached unreachable code"
        in
        let from_x = Int.of_string from_x in
        let from_y = Int.of_string from_y in
        let to_x = Int.of_string to_x in
        let to_y = Int.of_string to_y in
        Ok (create action from_x from_y to_x to_y)
    | _ -> Error (Error.of_string "Unable to parse instruction")

  let t_list_of_string_list ~mode strlist =
    List.map strlist ~f:(fun x -> t_of_string ~mode x) |> Result.all
end

module Grid = struct
  type t = { size : int; lamps : int array }

  let create size = { size; lamps = Array.create ~len:(size * size) 0 }
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
    | Instruction.Switch_on -> update grid rect (fun _ -> 1)
    | Instruction.Switch_off -> update grid rect (fun _ -> 0)
    | Instruction.Toggle -> update grid rect (fun x -> if x = 1 then 0 else 1)
    | Instruction.Increase -> update grid rect (fun x -> x + 1)
    | Instruction.Decrease ->
        update grid rect (fun x -> if x = 0 then 0 else x - 1)
    | Instruction.Increase_double -> update grid rect (fun x -> x + 2)

  let count_lit grid = Array.count grid.lamps ~f:(fun x -> x > 0)
  let total_brightness grid = Array.fold ~init:0 ~f:( + ) grid.lamps
end

let execute ~mode raw_instructions =
  match Instruction.t_list_of_string_list ~mode raw_instructions with
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
      (match execute ~mode:Standard raw_instructions with
      | Ok finished_grid ->
          Stdio.printf "In standard mode, %d lights are lit.\n"
            (Grid.count_lit finished_grid)
      | Error err ->
          Stdio.printf "Unable to run program: %s\n" (Error.to_string_hum err));

      match execute ~mode:Elvish raw_instructions with
      | Ok finished_grid ->
          Stdio.printf "In Elvish mode, the total brightness is %d.\n"
            (Grid.total_brightness finished_grid)
      | Error err ->
          Stdio.printf "Unable to run program: %s\n" (Error.to_string_hum err))
