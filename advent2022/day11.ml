open Base
open Stdio

let day = 11

let tests () =
  let input = In_channel.read_all (Utils.extra_path "day11test.txt") in
  match Monkey.parse_monkeys input with
  | Error err ->
      printf "Unable to read test monkeys because ‹%s›.\n"
        (Sexp.to_string (Error.sexp_of_t err))
  | Ok monkeys ->
      (* let final_monkeys = Monkey.iterate_times 20 monkeys in *)
      (* printf "The test final monkey business is %d.\n" *)
      (*   (Monkey.monkey_business final_monkeys); *)
      let panicked = Monkey.iterate_times ~panic:true 10000 monkeys in
      printf "test WIth panic and 10000 rounds, the monkey business is %d.\n"
        (Monkey.monkey_business panicked)

let run () =
  tests ();
  let input = In_channel.read_all (Utils.input_path_for_day 11) in
  match Monkey.parse_monkeys input with
  | Error err ->
      printf "Unable to read monkeys because ‹%s›.\n"
        (Sexp.to_string (Error.sexp_of_t err))
  | Ok monkeys ->
      (* let final_monkeys = Monkey.iterate_times 20 monkeys in *)
      (* printf "The final monkey business is %d.\n" *)
      (*   (Monkey.monkey_business final_monkeys); *)

      let monkeys_with_panic = Monkey.iterate_times ~panic:true 10000 monkeys in
      printf
        "With panicking and after 10000 iterations, the monkey business is %d.\n"
        (Monkey.monkey_business monkeys_with_panic)
