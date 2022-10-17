open Base

let unwrap = function Ok x -> x | Error err -> failwith err

let magnitudes =
  [
    (1_000_000_000L, "billion");
    (1_000_000L, "million");
    (1_000L, "thousand");
    (100L, "hundred");
  ]

let rec english_components number =
  let open Int64 in
  match number with
  | 0L -> Ok [ "zero" ]
  | 1L -> Ok [ "one" ]
  | 2L -> Ok [ "two" ]
  | 3L -> Ok [ "three" ]
  | 4L -> Ok [ "four" ]
  | 5L -> Ok [ "five" ]
  | 6L -> Ok [ "six" ]
  | 7L -> Ok [ "seven" ]
  | 8L -> Ok [ "eight" ]
  | 9L -> Ok [ "nine" ]
  | 10L -> Ok [ "ten" ]
  | 11L -> Ok [ "eleven" ]
  | 12L -> Ok [ "twelve" ]
  | 13L -> Ok [ "thirteen" ]
  | 14L -> Ok [ "fourteen" ]
  | 15L -> Ok [ "fifteen" ]
  | 16L -> Ok [ "sixteen" ]
  | 17L -> Ok [ "seventeen" ]
  | 18L -> Ok [ "eighteen" ]
  | 19L -> Ok [ "nineteen" ]
  | 20L -> Ok [ "twenty" ]
  | 30L -> Ok [ "thirty" ]
  | 40L -> Ok [ "forty" ]
  | 50L -> Ok [ "fifty" ]
  | 60L -> Ok [ "sixty" ]
  | 70L -> Ok [ "seventy" ]
  | 80L -> Ok [ "eighty" ]
  | 90L -> Ok [ "ninety" ]
  | _ ->
      if number > 999_999_999_999L || number < 0L then
        Error "input out of range"
      else if 20L < number && number < 100L then
        (* decades *)
        Ok
          [
            String.concat
              [
                List.hd_exn (unwrap (english_components (number / 10L * 10L)));
                "-";
                List.hd_exn (unwrap (english_components (number % 10L)));
              ];
          ]
      else
        (* magnitudes *)
        let remainder = ref number in
        let magnitude_components =
          List.filter_map magnitudes ~f:(fun (mag_num, mag_word) ->
              let factor = !remainder / mag_num in
              if factor > 0L then (
                let result =
                  List.concat
                    [ unwrap (english_components factor); [ mag_word ] ]
                in
                remainder := !remainder % mag_num;
                Some result)
              else None)
          |> List.concat
        in
        let remainder = number % 100L in
        Ok
          (List.concat
             [
               magnitude_components;
               (if remainder <> 0L then unwrap (english_components remainder)
               else []);
             ])

let in_english number =
  match english_components number with
  | Ok components -> Ok (String.concat ~sep:" " components)
  | Error err -> Error err
