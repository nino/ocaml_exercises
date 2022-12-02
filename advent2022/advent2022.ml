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
  ()
