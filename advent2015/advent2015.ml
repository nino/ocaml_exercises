module type AdventDay = sig
  val run : unit -> unit
end

let call_and_flush (module M : AdventDay) =
  M.run ();
  (* `%!` flushes, I think?? *)
  Stdio.printf "\n%!"

let () = Stdio.printf "Yo\n"
let () = call_and_flush (module Day2)
let () = call_and_flush (module Day3)
let () = call_and_flush (module Day4)
let () = call_and_flush (module Day5)
