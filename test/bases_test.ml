(* all-your-base - 2.3.0 *)
open OUnit2
open Exs.Bases

let option_printer = function
  | None -> "None"
  | Some xs -> "Some [" ^ String.concat ";" (List.map string_of_int xs) ^ "]"

let ae exp got _test_ctxt = assert_equal exp got ~printer:option_printer
let ae_int exp got _text_ctxt = assert_equal exp got ~printer:Int.to_string

let tests =
  [
    "digit 0 of 123 in base 10" >:: ae_int 3 (digit ~exponent:0 ~base:10 123);
    "digit 1 of 123 in base 10" >:: ae_int 2 (digit ~exponent:1 ~base:10 123);
    "digit 2 of 123 in base 10" >:: ae_int 1 (digit ~exponent:2 ~base:10 123);
    "num_digits 0 base 10" >:: ae_int 1 (num_digits ~base:10 0);
    "num_digits 1 base 10" >:: ae_int 1 (num_digits ~base:10 1);
    "num_digits 2 base 10" >:: ae_int 1 (num_digits ~base:10 2);
    "num_digits 3 base 10" >:: ae_int 1 (num_digits ~base:10 3);
    "num_digits 33 base 10" >:: ae_int 2 (num_digits ~base:10 33);
    "num_digits 9 base 2" >:: ae_int 4 (num_digits ~base:2 9);
    "format 1" >:: ae (Some [ 1 ]) (format ~base:10 1);
    "format 2" >:: ae (Some [ 2 ]) (format ~base:10 2);
    "format 123" >:: ae (Some [ 1; 2; 3 ]) (format ~base:10 123);
    "single bit one to decimal"
    >:: ae (Some [ 1 ]) (convert_bases ~from:2 ~digits:[ 1 ] ~target:10);
    "binary to single decimal"
    >:: ae (Some [ 5 ]) (convert_bases ~from:2 ~digits:[ 1; 0; 1 ] ~target:10);
    "single decimal to binary"
    >:: ae (Some [ 1; 0; 1 ]) (convert_bases ~from:10 ~digits:[ 5 ] ~target:2);
    "binary to multiple decimal"
    >:: ae
          (Some [ 4; 2 ])
          (convert_bases ~from:2 ~digits:[ 1; 0; 1; 0; 1; 0 ] ~target:10);
    "decimal to binary"
    >:: ae
          (Some [ 1; 0; 1; 0; 1; 0 ])
          (convert_bases ~from:10 ~digits:[ 4; 2 ] ~target:2);
    "trinary to hexadecimal"
    >:: ae
          (Some [ 2; 10 ])
          (convert_bases ~from:3 ~digits:[ 1; 1; 2; 0 ] ~target:16);
    "hexadecimal to trinary"
    >:: ae
          (Some [ 1; 1; 2; 0 ])
          (convert_bases ~from:16 ~digits:[ 2; 10 ] ~target:3);
    "15-bit integer"
    >:: ae
          (Some [ 6; 10; 45 ])
          (convert_bases ~from:97 ~digits:[ 3; 46; 60 ] ~target:73);
    "empty list"
    >:: ae (Some [ 0 ]) (convert_bases ~from:2 ~digits:[] ~target:10);
    "single zero"
    >:: ae (Some [ 0 ]) (convert_bases ~from:10 ~digits:[ 0 ] ~target:2);
    "multiple zeros"
    >:: ae (Some [ 0 ]) (convert_bases ~from:10 ~digits:[ 0; 0; 0 ] ~target:2);
    "leading zeros"
    >:: ae
          (Some [ 4; 2 ])
          (convert_bases ~from:7 ~digits:[ 0; 6; 0 ] ~target:10);
    "input base is one"
    >:: ae None (convert_bases ~from:1 ~digits:[ 0 ] ~target:10);
    "input base is zero"
    >:: ae None (convert_bases ~from:0 ~digits:[] ~target:10);
    "input base is negative"
    >:: ae None (convert_bases ~from:(-2) ~digits:[ 1 ] ~target:10);
    "negative digit"
    >:: ae None (convert_bases ~from:2 ~digits:[ 1; -1; 1; 0; 1; 0 ] ~target:10);
    "invalid positive digit"
    >:: ae None (convert_bases ~from:2 ~digits:[ 1; 2; 1; 0; 1; 0 ] ~target:10);
    "output base is one"
    >:: ae None (convert_bases ~from:2 ~digits:[ 1; 0; 1; 0; 1; 0 ] ~target:1);
    "output base is zero"
    >:: ae None (convert_bases ~from:10 ~digits:[ 7 ] ~target:0);
    "output base is negative"
    >:: ae None (convert_bases ~from:2 ~digits:[ 1 ] ~target:(-7));
    "both bases are negative"
    >:: ae None (convert_bases ~from:(-2) ~digits:[ 1 ] ~target:(-7));
  ]

let run_test () = run_test_tt_main ("all-your-bases tests" >::: tests)
