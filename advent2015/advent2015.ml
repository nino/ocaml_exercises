let call_and_flush f =
  f ();
  Stdio.printf "\n";
  Stdio.Out_channel.flush Stdio.Out_channel.stdout

let () = Stdio.printf "Yo\n"
let () = call_and_flush Day2.run
let () = call_and_flush Day3.run
let () = call_and_flush Day4.run
let () = call_and_flush Day5.run
