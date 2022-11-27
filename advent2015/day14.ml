open ContainersLabels

type reindeer = {
  name : string;
  speed : int;
  flying_time : int;
  resting_time : int;
}

module ReindeerMap = Map.Make (String)

let parse line =
  let pattern =
    Re.compile
    @@ Re.Pcre.re
         {|(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.|}
  in
  match Re.exec pattern line |> Re.Group.all with
  | [| _all; name; speed; flying_time; resting_time |] ->
      Some
        {
          name;
          speed = int_of_string speed;
          flying_time = int_of_string flying_time;
          resting_time = int_of_string resting_time;
        }
  | _ -> None

let distance_after ~sec reindeer =
  let cycle_time = reindeer.flying_time + reindeer.resting_time in
  let cycle_distance = reindeer.flying_time * reindeer.speed in
  let completed_cycles = sec / cycle_time in
  let completed_cycles_dist = completed_cycles * cycle_distance in
  let extra_seconds = Int.min reindeer.flying_time (sec mod cycle_time) in
  let extra_dist = extra_seconds * reindeer.speed in
  completed_cycles_dist + extra_dist

let tests () =
  let p = Printf.printf in
  let comet =
    { name = "Comet"; speed = 14; flying_time = 10; resting_time = 127 }
  in
  let dancer =
    { name = "Dancer"; speed = 16; flying_time = 11; resting_time = 162 }
  in
  p "%d should be %d\n" (distance_after ~sec:1 comet) 14;
  p "%d should be %d\n" (distance_after ~sec:1 dancer) 16;
  p "%d should be %d\n" (distance_after ~sec:10 comet) 140;
  p "%d should be %d\n" (distance_after ~sec:10 dancer) 160;
  p "%d should be %d\n" (distance_after ~sec:11 comet) 140;
  p "%d should be %d\n" (distance_after ~sec:11 dancer) 176;
  p "%d should be %d\n" (distance_after ~sec:1000 comet) 1120;
  p "%d should be %d\n" (distance_after ~sec:1000 dancer) 1056

let winners_after_seconds ~sec reindeers =
  let max_distance = ref 0 in
  let distances =
    List.map reindeers ~f:(fun reindeer ->
        let distance = distance_after reindeer ~sec in
        max_distance := Int.max distance !max_distance;
        (reindeer, distance))
  in
  List.filter_map distances ~f:(fun (deer, dist) ->
      if dist = !max_distance then Some (deer, dist) else None)

let run () =
  Utils.greet 14;

  tests ();

  let lines = IO.(with_in (Utils.input_path_for_day 14) read_lines_l) in
  let reindeers = List.filter_map lines ~f:parse in
  let winners = winners_after_seconds ~sec:2503 reindeers in
  (match winners with
  | (deer, dist) :: _tl ->
      Printf.printf "The winner is %s and they flew %d km.\n%!" deer.name dist
  | _ ->
      Printf.printf
        "We didn't find a winner, presumably because there were no deer in the \
         race.\n\
         %!");

  let scores = Hashtbl.create (List.length reindeers) in
  for i = 1 to 2503 do
    let winners = winners_after_seconds reindeers ~sec:i in
    List.iter winners ~f:(fun (name, _dist) -> Hashtbl.incr scores name)
  done;
  let winner, winner_score =
    Hashtbl.to_list scores
    |> List.reduce_exn ~f:(fun (best_name, best_score) (name, score) ->
           if score > best_score then (name, score) else (best_name, best_score))
  in
  Printf.printf "The final winner is %s and their score is %d.\n%!" winner.name
    winner_score;

  ()
