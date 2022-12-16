open Base

module SantaPosition = struct
  type t = int * int [@@deriving compare, sexp_of, hash]
end

let visit recipients current_position =
  match Hashtbl.find recipients current_position with
  | Some num -> Hashtbl.set recipients ~key:current_position ~data:(num + 1)
  | None -> Hashtbl.set recipients ~key:current_position ~data:1

let update_position pos direction =
  let x, y = !pos in
  match direction with
  | '^' -> pos := (x, y + 1)
  | 'v' -> pos := (x, y - 1)
  | '<' -> pos := (x + 1, y)
  | '>' -> pos := (x - 1, y)
  | _ -> ()

let process_input_part_1 input =
  let recipients = Hashtbl.create (module SantaPosition) in

  let current_position = ref (0, 0) in

  (* Visit the first house *)
  visit recipients !current_position;

  String.iter input ~f:(fun direction ->
      update_position current_position direction;
      visit recipients !current_position);
  Stdio.printf "Santa visited %d houses\n" (Hashtbl.length recipients)

let process_input_part_2 input =
  let recipients = Hashtbl.create (module SantaPosition) in

  let current_position_santa = ref (0, 0) in
  let current_position_robo = ref (0, 0) in
  let step = ref 0 in

  (* Visit the first house *)
  visit recipients !current_position_santa;

  String.iter input ~f:(fun direction ->
      (* It’s a bit silly to do the parity check on every step. We could speed
         this up by having one loop that only has (String.length input) / 2 steps,
         but then I’d have to separately deal with uneven-length strings. *)
      if !step % 2 = 0 then (
        update_position current_position_santa direction;
        visit recipients !current_position_santa)
      else (
        update_position current_position_robo direction;
        visit recipients !current_position_robo);
      Int.incr step);
  Stdio.printf "Santa and robo visited %d houses together\n"
    (Hashtbl.length recipients)

let run () =
  Utils.greet 3;

  let input_path = Utils.input_path_for_day 3 in
  match Stdio.In_channel.read_all input_path with
  | input ->
      process_input_part_1 input;
      process_input_part_2 input
  | exception _ -> Stdio.printf "Unable to read input file for some reason.\n"
