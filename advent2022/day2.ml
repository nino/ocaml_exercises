open Base
open Stdio

type move = Rock | Paper | Scissors

let decrypt_move = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> failwith "Unknown move"

let winning_move opponent =
  match opponent with Rock -> Paper | Paper -> Scissors | Scissors -> Rock

let drawing_move opponent = opponent

let losing_move opponent =
  match opponent with Paper -> Rock | Scissors -> Paper | Rock -> Scissors

let determine_moves2 opponent me =
  let opponent = decrypt_move opponent in
  match me with
  | 'X' -> (opponent, losing_move opponent)
  | 'Y' -> (opponent, drawing_move opponent)
  | 'Z' -> (opponent, winning_move opponent)
  | _ -> failwith "Unknown character"

let decrypt_line ?(part2 = false) line =
  let chars = String.to_array line in
  match chars with
  | [| opponent; ' '; me |] ->
      if not part2 then (decrypt_move opponent, decrypt_move me)
      else determine_moves2 opponent me
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

  let rounds' = List.map input ~f:(decrypt_line ~part2:true) in
  let my_score =
    List.fold rounds' ~init:0 ~f:(fun sum (opponent, me) ->
        sum + score opponent me)
  in
  printf "With the new strategy, my score is %d.\n" my_score;
  ()
