open Base

let is_word_char char = Char.(is_alphanum char || char = '\'')
let trim_apostrophes word = String.strip ~drop:(Char.equal '\'') word

let words sentence =
  let normalize_word word = word |> String.lowercase |> trim_apostrophes in

  (* Arbitrarily initialize the buffer with capacity 8, so many words won't have to
     reallocate. *)
  let wip_word = Buffer.create 8 in

  (* We're adding the words to the accumulator in reverse order but that doesn't
     matter *)
  let words_except_the_last_one =
    String.fold sentence ~init:[] ~f:(fun completed_words next_char ->
        if is_word_char next_char then (
          Buffer.add_char wip_word next_char;
          completed_words)
        else
          let completed_words =
            if Buffer.length wip_word = 0 then completed_words
            else (Buffer.contents wip_word |> normalize_word) :: completed_words
          in
          Buffer.clear wip_word;
          completed_words)
  in

  (* This is kind of ugly: The `String.fold` won't know to add the last word to
     the list of words, so we need to put the last straggler in manually. *)
  if Buffer.length wip_word = 0 then words_except_the_last_one
  else (Buffer.contents wip_word |> normalize_word) :: words_except_the_last_one

let freqs words =
  List.fold words
    ~init:(Map.empty (module String))
    ~f:(fun freqs word ->
      match Map.find freqs word with
      | Some count -> Map.set freqs ~key:word ~data:(count + 1)
      | None -> Map.set freqs ~key:word ~data:1)

let word_count sentence =
  let words = words sentence in
  freqs words
