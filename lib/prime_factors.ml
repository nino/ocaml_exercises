open Base

let not_divisible a b = Int64.(a % b <> 0L)

let do_int64_seq length ~f =
  let open Int64 in
  let index = ref 0L in
  while !index < length do
    f index.contents;
    index.contents <- index.contents + 1L
  done

(* Use that sieve from CS *)
let primes_less_than n =
  let open Int64 in
  let target = n / 2L in
  let primes = Hash_set.create (module Int64) in
  do_int64_seq target ~f:(fun i ->
      if Hash_set.for_all primes ~f:(not_divisible (i + 2L)) then
        Hash_set.add primes (i + 2L)
      else ());
  primes |> Hash_set.to_list |> List.sort ~compare:(fun a b -> compare b a)

let factors_of n =
  let open Int64 in
  let primes = primes_less_than n in
  (* List.iter primes ~f:(fun prime -> Stdio.printf "%d, " (to_int_exn prime)); *)
  (* Stdio.printf "\n\n"; *)
  let rec factors_of' factors remaining_primes n =
    match remaining_primes with
    | [] -> factors
    | hd :: tl ->
        if n % hd = 0L then factors_of' (hd :: factors) (hd :: tl) (n / hd)
        else factors_of' factors tl n
  in
  factors_of' [] primes n
