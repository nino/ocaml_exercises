module type AdventDay = sig
  val run : unit -> unit
  val day : int
end

let run (module M : AdventDay) =
  (try
     Stdio.printf "Advent of Code 2022, Day %d!\n" M.day;
     M.run ()
   with ex ->
     Stdio.printf "Failed to run day %d, because: %s\n" M.day
       (Printexc.to_string ex));
  Stdio.printf "\n%!"

let () =
  Stdio.printf "\n\n";
  run (module Day1);
  run (module Day2);
  run (module Day3);
  run (module Day4);
  run (module Day5);
  run (module Day6);
  run (module Day7);
  run (module Day8);
  run (module Day9);
  run (module Day10)
