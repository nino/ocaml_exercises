(* Okay, this is stupid: Apparently `List.map` isn't properly tail-recursive, so
   you get a stack-overflow in `iterate`. So I guess I'll be opening Containers
   from now on. *)
open Containers

let pattern = Re.compile @@ Re.Pcre.re {|(1+|2+|3+|4+|5+|6+|7+|8+|9+|0+)|}

let iterate input =
  let sequences =
    Re.all pattern input |> ListLabels.map ~f:(fun g -> Re.Group.get g 0)
  in
  ListLabels.map sequences ~f:(fun seq ->
      string_of_int (String.length seq) ^ String.sub seq 0 1)
  |> String.concat ""

let run () =
  Utils.greet 10;
  let input = "1113222113" in
  let result = ref input in
  let num_iterations = 40 in
  for _ = 1 to num_iterations do
    result := iterate !result
  done;
  Printf.printf "After %d iterations, the string is %d characters long.\n%!"
    num_iterations (String.length !result);

  let result = ref input in
  let num_iterations = 50 in
  for i = 1 to num_iterations do
    Printf.printf
      "Current result is %d long.\n\
       Here's a sample: %s.\n\
       Doing iteration %d...\n\
       %!"
      (String.length !result)
      (String.sub !result 0 (Int.min (String.length !result) 100))
      i;
    result := iterate !result
  done;
  Printf.printf "After %d iterations, the string is %d characters long.\n%!"
    num_iterations (String.length !result)
