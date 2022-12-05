open Base
open Stdio

let day = 5

let parse_header_exn str =
  let lines = String.split_lines str |> Array.of_list in
  let grid = Array.map lines ~f:String.to_array in
  let transposed = Array.transpose_exn grid |> Array.map ~f:Array.rev in
  let stacks = Hashtbl.create (module Int) in
  Array.iter transposed ~f:(fun line ->
      match Char.get_digit line.(0) with
      | Some key ->
          let stack = Stack.create () in
          Array.iter
            (Array.sub line ~pos:1 ~len:(Array.length line - 1))
            ~f:(fun char ->
              if Char.is_alpha char then Stack.push stack char else ());
          Hashtbl.set stacks ~key ~data:stack
      | None -> ());
  stacks

let parse_instructions_exn str =
  let lines = String.split_lines str in
  List.map lines ~f:(fun line ->
      match String.split ~on:' ' line with
      | [ "move"; count; "from"; source; "to"; dest ] ->
          (Int.of_string count, Int.of_string source, Int.of_string dest)
      | _ -> failwith "Invalid line")

let parse_input_exn input =
  match
    String.Search_pattern.split_on (String.Search_pattern.create "\n\n") input
  with
  | [ header; rest ] -> (parse_header_exn header, parse_instructions_exn rest)
  | _ -> failwith "Invalid input"

let execute stacks instructions =
  List.iter instructions ~f:(fun (count, source, dest) ->
      for _ = 1 to count do
        let crate = Stack.pop_exn (Hashtbl.find_exn stacks source) in
        Stack.push (Hashtbl.find_exn stacks dest) crate
      done)

let run () =
  let input = In_channel.read_all (Utils.input_path_for_day 5) in
  let stacks, instructions = parse_input_exn input in
  execute stacks instructions;
  let top_crates =
    List.init (Hashtbl.length stacks) ~f:(fun idx ->
        Stack.top_exn (Hashtbl.find_exn stacks (idx + 1)))
  in
  printf "The top crates are %s.\n" (String.of_char_list top_crates);
  ()
