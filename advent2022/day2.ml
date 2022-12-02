open Base
open Stdio

type move = Rock | Paper | Scissors

let decrypt_move = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> failwith "Unknown move"

let decrypt_line line =
  let chars = String.to_array line in
  match chars with
  | [| opponent; ' '; me |] -> (decrypt_move opponent, decrypt_move me)
  | _ -> failwith "Invalid line"

let score opponent me =
  match me with
  | Rock -> (
      match opponent with Paper -> 1 + 0 | Rock -> 1 + 3 | Scissors -> 1 + 6)
  | Paper -> (
      match opponent with Scissors -> 2 + 0 | Paper -> 2 + 3 | Rock -> 2 + 6)
  | Scissors -> (
      match opponent with Rock -> 3 + 0 | Scissors -> 3 + 3 | Paper -> 3 + 6)

let day = 2

let run () =
  let input = In_channel.read_lines (Utils.input_path_for_day 2) in
  let rounds = List.map input ~f:decrypt_line in
  let my_score =
    List.fold rounds ~init:0 ~f:(fun sum (opponent, me) ->
        sum + score opponent me)
  in
  printf "My score is %d.\n" my_score;
  ()
