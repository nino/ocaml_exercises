open Base

module Box = struct
  type t = { width : int; height : int; depth : int }
  [@@deriving compare, sexp_of, hash]

  let parse input =
    let components =
      input |> String.strip |> String.split ~on:'x'
      |> List.map ~f:(fun s -> try Ok (Int.of_string s) with _ -> Error "...")
    in
    match components with
    | [ Ok width; Ok height; Ok depth ] -> Ok { width; height; depth }
    | _ -> Error ("invalid box spec: ‹" ^ input ^ "›")

  let surface_area { width; height; depth } =
    (2 * (width * height)) + (2 * (width * depth)) + (2 * (depth * height))

  let smallest_side_area { width; height; depth } =
    List.reduce_exn ~f:Int.min [ width * height; width * depth; height * depth ]

  let smallest_side_perimeter { width; height; depth } =
    List.reduce_exn ~f:Int.min
      [ 2 * (width + height); 2 * (width + depth); 2 * (height + depth) ]

  let paper_required box = surface_area box + smallest_side_area box

  let process input =
    match parse input with
    | Ok box -> Ok (paper_required box)
    | Error err -> Error err

  let boxes_of_strings strings =
    let rec loop acc strings =
      match acc with
      | Error err -> Error err
      | Ok boxes -> (
          match strings with
          | [] -> Ok boxes
          | hd :: tl -> (
              match parse hd with
              | Ok box -> loop (Ok (box :: boxes)) tl
              | Error err -> Error err))
    in
    loop (Ok []) strings

  let volume { width; height; depth } = width * height * depth
  let ribbon_required box = smallest_side_perimeter box + volume box
end

let process_inputs inputs =
  let rec loop acc inputs =
    match acc with
    | Ok total -> (
        match inputs with
        | [] -> Ok total
        | hd :: tl -> (
            match Box.process hd with
            | Ok paper -> loop (Ok (total + paper)) tl
            | Error err -> Error err))
    | Error err -> Error err
  in
  loop (Ok 0) inputs

let sum_ints = List.fold ~init:0 ~f:Int.( + )

let run () =
  Utils.greet 2;

  let input_path = Utils.input_path_for_day 2 in
  match Stdio.In_channel.read_lines input_path with
  | exception ex -> Stdio.printf "%s\n" (Exn.to_string ex)
  | inputs -> (
      match Box.boxes_of_strings inputs with
      | Error err -> Stdio.printf "Failed because %s\n" err
      | Ok boxes ->
          let total_paper =
            boxes |> List.map ~f:Box.paper_required |> sum_ints
          in
          Stdio.printf "You need %d sqft of paper\n" total_paper;

          let total_ribbon =
            boxes |> List.map ~f:Box.ribbon_required |> sum_ints
          in
          Stdio.printf "You need %d ft of ribbon\n" total_ribbon)
