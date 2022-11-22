module Connection = struct
  type source = Constant of int | Identifier of string

  type t =
    | Direct of source
    | And of source * source
    | Or of source * source
    | Lshift of source * int
    | Rshift of source * int
    | Not of source

  (* This currently won't work with cyclic structures *)
  let eval connections_map identifier =
    let open Result.Let_syntax in
    let known_values = Hashtbl.create (module String) in
    let rec eval' source =
      match source with
      | Constant n -> Ok n
      | Identifier id -> (
          match Hashtbl.find known_values id with
          | Some value -> Ok value
          | None ->
              let%bind value =
                match Map.find connections_map id with
                | None -> Error (Error.of_string "ssssssss")
                | Some (Direct a) -> eval' a
                | Some (And (a, b)) ->
                    let%bind a = eval' a in
                    let%bind b = eval' b in
                    Ok (a land b)
                | Some (Or (a, b)) ->
                    let%bind a = eval' a in
                    let%bind b = eval' b in
                    Ok (a lor b)
                | Some (Lshift (a, b)) ->
                    let%bind a = eval' a in
                    Ok (a lsl b)
                | Some (Rshift (a, b)) ->
                    let%bind a = eval' a in
                    Ok (a lsr b)
                | Some (Not a) ->
                    let%bind a = eval' a in
                    Ok (lnot a)
              in
              Hashtbl.set known_values ~key:id ~data:value;
              Ok value)
    in
    eval' (Identifier identifier)

  let source_of_string string =
    match Int.of_string string with
    | exception _ -> Identifier string
    | number -> Constant number

  let of_string string =
    let open Result.Let_syntax in
    let components =
      String.split ~on:' ' string
      |> List.filter ~f:(fun el -> String.(el <> ""))
    in
    match components with
    | [ source; "->"; dest ] -> Ok (Direct (source_of_string source), dest)
    | [ "NOT"; source; "->"; dest ] -> Ok (Not (source_of_string source), dest)
    | [ a; "AND"; b; "->"; dest ] ->
        Ok (And (source_of_string a, source_of_string b), dest)
    | [ a; "OR"; b; "->"; dest ] ->
        Ok (Or (source_of_string a, source_of_string b), dest)
    | [ a; "LSHIFT"; b; "->"; dest ] ->
        let%bind b = Or_error.try_with (fun () -> Int.of_string b) in
        Ok (Lshift (source_of_string a, b), dest)
    | [ a; "RSHIFT"; b; "->"; dest ] ->
        let%bind b = Or_error.try_with (fun () -> Int.of_string b) in
        Ok (Rshift (source_of_string a, b), dest)
    | _ -> Error (Error.of_string "Unrecognized format")
end

let run () =
  Utils.greet 7;
  let input_path = Utils.input_path_for_day 7 in
  match Stdio.In_channel.read_lines input_path with
  | exception ex ->
      Stdio.printf "Error reading input: %s\n%!" (Exn.to_string ex)
  | lines -> (
      match List.map lines ~f:Connection.of_string |> Result.all with
      | Error err ->
          Stdio.printf "Encountered error: %s\n" (Error.to_string_hum err)
      | Ok connections -> (
          let connections_map =
            List.map connections ~f:(fun (connection, dest) ->
                (dest, connection))
            |> Map.of_alist_exn (module String)
          in
          match Connection.eval connections_map "a" with
          | Error err -> Stdio.printf "Error: %s\n%!" (Error.to_string_hum err)
          | Ok value -> (
              Stdio.printf "The value for ‹a› is %d.\n%!" value;

              let connections_map2 =
                Map.set connections_map ~key:"b"
                  ~data:(Connection.Direct (Connection.Constant value))
              in
              Stdio.printf "yo ok\n%!";
              match Connection.eval connections_map2 "a" with
              | Ok value -> Stdio.printf "The value for ‹a› is %d.\n%!" value
              | Error err ->
                  Stdio.printf "Error: %s\n%!" (Error.to_string_hum err))))
