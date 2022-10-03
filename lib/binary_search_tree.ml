open Base

type bst = Empty | Valid of { value : int; below : bst; above : bst }

let empty : bst = Empty
let option_to_result = function Valid x -> Ok (Valid x) | Empty -> Error "wtf"

let value tree =
  match tree with Valid { value; _ } -> Ok value | Empty -> Error "wtf"

let left = function
  | Valid { below; _ } -> option_to_result below
  | Empty -> Error "wtf"

let right = function
  | Valid { above; _ } -> option_to_result above
  | Empty -> Error "wtf"

let rec insert value = function
  | Empty -> Valid { value; below = empty; above = empty }
  | Valid node ->
      if value > node.value then
        Valid { node with above = insert value node.above }
      else Valid { node with below = insert value node.below }

let rec to_list = function
  | Empty -> []
  | Valid { value; above; below } ->
      List.concat [ to_list below; [ value ]; to_list above ]
