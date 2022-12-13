open Base
open Stdio

let day = 10

module CPU = struct
  type instruction = Noop | Prep | Add_x of int [@@deriving sexp_of]

  type t = { cycle : int; x : int; program : instruction list }
  [@@deriving sexp_of]

  let init_with_program program = { cycle = 1; x = 1; program }

  let compile_exn lines =
    List.concat_map lines ~f:(fun line ->
        match String.split ~on:' ' line with
        | [ "noop" ] -> [ Noop ]
        | [ "addx"; amount ] -> [ Prep; Add_x (Int.of_string amount) ]
        | _ -> failwith "Unrecognized instruction")

  let step computer =
    match computer.program with
    | [] -> { computer with cycle = computer.cycle + 1 }
    | Noop :: tl -> { computer with cycle = computer.cycle + 1; program = tl }
    | Prep :: tl -> { computer with cycle = computer.cycle + 1; program = tl }
    | Add_x n :: tl ->
        { cycle = computer.cycle + 1; program = tl; x = computer.x + n }

  let rec run_to_cycle computer target_cycle =
    if computer.cycle >= target_cycle then computer
    else run_to_cycle (step computer) target_cycle

  let signal computer = computer.cycle * computer.x

  let get_sum_of_signals computer =
    let computer' = ref computer in
    let sum = ref 0 in
    let cycle_nums = [ 20; 60; 100; 140; 180; 220 ] in
    List.iter cycle_nums ~f:(fun cycle ->
        computer' := run_to_cycle !computer' cycle;
        sum := !sum + signal !computer');
    !sum

  let print_program_output computer =
    let computer = ref computer in
    for _ = 1 to 240 do
      let cursor = !computer.cycle % 40 in
      if
        cursor = !computer.x
        || cursor = !computer.x + 1
        || cursor = !computer.x - 1
      then printf "#"
      else printf ".";
      if !computer.cycle % 40 = 0 then printf "\n";
      computer := step !computer
    done
end

let tests () =
  printf "Test 1 with small program.\n";
  let input = [ "noop"; "addx 3"; "addx -5" ] in
  let program = CPU.compile_exn input in
  let computer = CPU.init_with_program program in
  printf "%d.\n" (CPU.get_sum_of_signals computer);

  printf "\n\ntest 2 with big program.\n";
  let input = In_channel.read_lines (Utils.extra_path "day10_test2.txt") in
  let program = CPU.compile_exn input in
  let computer = CPU.init_with_program program in
  printf "%d should be 13140.\n" (CPU.get_sum_of_signals computer);
  CPU.print_program_output computer

let run () =
  tests ();
  let input = In_channel.read_lines (Utils.input_path_for_day 10) in
  let program = CPU.compile_exn input in
  let computer = CPU.init_with_program program in
  printf "The sum of the signal strengths is %d.\n"
    (CPU.get_sum_of_signals computer);

  printf "The output for the program is not working, but here's what we get:\n";
  CPU.print_program_output computer
