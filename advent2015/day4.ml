let does_number_produce_valid_hash ?(extra_hard = false) secret_key number =
  let hash_input = secret_key ^ Int.to_string number in
  let hex = hash_input |> Md5_lib.string |> Md5_lib.to_hex in
  String.is_prefix hex ~prefix:(if extra_hard then "000000" else "00000")

let mine_coin ?(extra_hard = false) secret_key =
  let rec loop counter =
    if does_number_produce_valid_hash ~extra_hard secret_key counter then
      counter
    else loop (counter + 1)
  in
  loop 1

let run () =
  Utils.greet 4;

  let input = "ckczppom" in
  let coin_number1 = mine_coin input in
  Stdio.printf "The easy coin number is %d.\n" coin_number1;
  let coin_number2 = mine_coin ~extra_hard:true input in
  Stdio.printf "The hard coin number is %d.\n" coin_number2
