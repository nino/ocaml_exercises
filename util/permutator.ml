module Make (S : Set.S) = struct
  let rec permutations s =
    if S.cardinal s = 1 then Seq.return (S.elements s)
    else
      let cities_seq = S.to_seq s in
      cities_seq
      |> Seq.flat_map (fun city ->
             let other_cities = S.remove city s in
             let permutations_of_others = permutations other_cities in
             permutations_of_others
             |> Seq.map (fun permutation -> city :: permutation))
end
