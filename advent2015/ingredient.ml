open ContainersLabels
module StringMap = Map.Make (String)

type t = {
  capacity : int;
  durability : int;
  flavor : int;
  texture : int;
  calories : int;
}

type cupboard = t StringMap.t

let empty_cupboard = StringMap.empty

let cupboard_add cupboard name ingredient =
  StringMap.add name ingredient cupboard

let cupboard_find_exn cupboard name = StringMap.find name cupboard

let ingredients_in_cupboard cupboard =
  StringMap.to_list cupboard |> List.map ~f:(fun (k, _v) -> k)

(* At some point I need to figure out how to derive this kind of stuff without
   Base *)
let sexp_of_t t =
  Sexp.of_list
    [
      `List [ `Atom "capacity"; `Atom (string_of_int t.capacity) ];
      `List [ `Atom "durability"; `Atom (string_of_int t.durability) ];
      `List [ `Atom "flavor"; `Atom (string_of_int t.flavor) ];
      `List [ `Atom "texture"; `Atom (string_of_int t.texture) ];
      `List [ `Atom "calories"; `Atom (string_of_int t.calories) ];
    ]
