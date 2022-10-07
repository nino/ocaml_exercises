type delim = Braces | Brackets | Parens

exception NotBalanced

let are_balanced input =
  let open Stack in
  let stack : delim t = create () in
  try
    input
    |> String.iter (function
         | '{' -> push Braces stack
         | '}' -> (
             match pop_opt stack with
             | None -> raise NotBalanced
             | Some Braces -> ()
             | Some _ -> raise NotBalanced)
         | '(' -> push Parens stack
         | ')' -> (
             match pop_opt stack with
             | None -> raise NotBalanced
             | Some Parens -> ()
             | Some _ -> raise NotBalanced)
         | '[' -> push Brackets stack
         | ']' -> (
             match pop_opt stack with
             | None -> raise NotBalanced
             | Some Brackets -> ()
             | Some _ -> raise NotBalanced)
         | _ -> ());
    is_empty stack
  with _ -> false
