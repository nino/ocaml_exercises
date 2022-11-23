let source_code_bytes line = String.length line

type state = Normal | Escape_n of int

let memory_bytes line =
  let bytes = ref 0 in
  let state = ref Normal in
  let without_quotes = String.sub line 1 (String.length line - 2) in
  without_quotes
  |> String.iter (fun char ->
         match (!state, char) with
         | Escape_n 1, 'x' -> state := Escape_n 2
         | Escape_n 1, _ -> state := Normal
         | Escape_n n, _ -> state := Escape_n (n - 1)
         | Normal, '\\' ->
             incr bytes;
             state := Escape_n 1
         | Normal, _ -> incr bytes);
  !bytes

let size_of_source_minus_size_of_data_exn lines =
  lines
  |> List.map (fun line -> source_code_bytes line - memory_bytes line)
  |> List.fold_left ( + ) 0

let tests () =
  Stdio.printf "%d should be 12\n"
    (size_of_source_minus_size_of_data_exn
       [ {|""|}; {|"abc"|}; {|"aaa\"aaa"|}; {|"\x27"|} ])

let run () =
  Utils.greet 8;

  tests ();
  let input_path = Utils.input_path_for_day 8 in
  match Stdio.In_channel.read_lines input_path with
  | exception ex ->
      Stdio.printf "Unable to read file ‹%s›, because: %s\n%!" input_path
        (Printexc.to_string ex)
  | lines ->
      let part1 = size_of_source_minus_size_of_data_exn lines in
      Stdio.printf "Part 1: The difference is %d\n%!" part1
