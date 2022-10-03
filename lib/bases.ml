type base = int

exception NegativeDigit
exception NegativeExponent
exception NegativeNumber

(* Should just get Int.pow from Base, but for reasons lost to the depths of
   time, I didn't feel like it, so here's a terrible implementation of pow. *)
let rec pow num exponent =
  if exponent < 0 then raise NegativeExponent
  else if exponent = 0 then 1
  else num * pow num (exponent - 1)

let parse ~base digits =
  if base < 2 then None
  else if digits = [] then Some 0
  else
    let rec parse' num_so_far digits =
      match digits with
      | [] -> Some num_so_far
      | hd :: tl ->
          if hd < 0 || hd >= base then None
          else parse' ((num_so_far * base) + hd) tl
    in
    parse' 0 digits

let num_digits ~base number =
  if number < 0 then raise NegativeNumber
  else if number = 0 then 1
  else
    let rec loop result remainder =
      match remainder with
      | 0 -> result
      | _ -> loop (result + 1) (remainder / base)
    in
    loop 0 number

let digit ~exponent ~base number =
  let remainder = number mod pow base (exponent + 1) in
  remainder / pow base exponent

let format ~base number =
  if base < 2 || number < 0 then None
  else if number = 0 then Some [ 0 ]
  else
    let num_digits = num_digits ~base number in
    let rec make_digits acc exponent =
      match exponent with
      | -1 -> acc
      | _ -> make_digits (digit ~exponent ~base number :: acc) (exponent - 1)
    in
    Some (List.rev (make_digits [] (num_digits - 1)))

let convert_bases ~from ~digits ~target =
  match parse ~base:from digits with
  | Some num -> format ~base:target num
  | None -> None
