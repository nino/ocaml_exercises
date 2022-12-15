open Base

type operand = Int of int | Old [@@deriving sexp_of]
type operation = Plus of operand | Times of operand [@@deriving sexp_of]

type t = {
  id : int;
  items : int list;
  operation : operation;
  divisible : int;
  throw_target_if_divisible : int;
  throw_target_if_not_divisible : int;
  inspections_done : int;
}
[@@deriving sexp_of]

type monkeys = {
  individuals : (int, t, Int.comparator_witness) Map.t;
  order : int list;
}

let r = Re.Perl.compile_pat
let group_get n group = Re.Group.get group n

let parse_operation_exn operator operand =
  let operand =
    try Int (Int.of_string (String.strip operand)) with _ -> Old
  in
  match operator with
  | "+" -> Plus operand
  | "*" -> Times operand
  | _ -> failwith "Unknown operand"

let parse_monkey string =
  try
    Ok
      {
        id =
          Re.exec (r {|Monkey (\d+):|}) string |> group_get 1 |> Int.of_string;
        items =
          Re.exec (r {|Starting items: ([0-9, ]+)|}) string
          |> group_get 1 |> String.split ~on:','
          |> List.map ~f:(fun s -> String.strip s |> Int.of_string);
        operation =
          (let matches =
             Re.exec (r {|Operation: new = old (\*|\+) ([^ ]+)|}) string
           in
           parse_operation_exn (group_get 1 matches) (group_get 2 matches));
        divisible =
          Re.exec (r {|Test: divisible by (\d+)|}) string
          |> group_get 1 |> Int.of_string;
        throw_target_if_divisible =
          Re.exec (r {|If true: throw to monkey (\d+)|}) string
          |> group_get 1 |> Int.of_string;
        throw_target_if_not_divisible =
          Re.exec (r {|If false: throw to monkey (\d+)|}) string
          |> group_get 1 |> Int.of_string;
        inspections_done = 0;
      }
  with ex ->
    Stdio.printf "Unable to parse monkey %s.\n%s\n" string (Exn.to_string ex);
    Error (Error.of_exn ex)

let parse_monkeys string =
  let open Result.Let_syntax in
  let%bind monkeys =
    String.Search_pattern.split_on
      (String.Search_pattern.create "\n\n")
      (String.strip string)
    |> List.map ~f:parse_monkey |> Result.all
  in
  let monkeymap, rev_order =
    List.fold monkeys
      ~init:(Map.empty (module Int), [])
      ~f:(fun (map, rev_order) monkey ->
        (Map.set map ~key:monkey.id ~data:monkey, monkey.id :: rev_order))
  in
  Ok { individuals = monkeymap; order = List.rev rev_order }

let do_op = function
  | Plus (Int n) -> ( + ) n
  | Times (Int n) -> ( * ) n
  | Plus Old -> ( * ) 2
  | Times Old -> fun old -> old * old

let throw_item monkeys source destination =
  let dest_monkey = Map.find_exn monkeys destination in
  let src_monkey = Map.find_exn monkeys source in
  match src_monkey.items with
  | [] -> failwith "This shouldn't happen."
  | item :: others ->
      monkeys
      |> Map.set ~key:destination
           ~data:
             {
               dest_monkey with
               items =
                 List.append dest_monkey.items
                   [ do_op src_monkey.operation item / 3 ];
             }
      |> Map.set ~key:source ~data:{ src_monkey with items = others }

let destination monkey item =
  if item % monkey.divisible = 0 then monkey.throw_target_if_divisible
  else monkey.throw_target_if_not_divisible

let log_inspection monkeys id =
  let monkey = Map.find_exn monkeys id in
  Map.set monkeys ~key:id
    ~data:{ monkey with inspections_done = monkey.inspections_done + 1 }

let rec monkey_do monkeys id =
  let monkey = Map.find_exn monkeys id in
  match monkey.items with
  | [] -> monkeys
  | hd :: _tl ->
      let monkeys = log_inspection monkeys id in
      let processed_worry = do_op monkey.operation hd / 3 in
      monkey_do (throw_item monkeys id (destination monkey processed_worry)) id

let iterate { individuals; order } =
  let individuals' =
    List.fold order ~init:individuals ~f:(fun individuals id ->
        monkey_do individuals id)
  in
  { individuals = individuals'; order }

let monkey_business { individuals; _ } =
  let rec loop (max1, max2) monkeylist =
    match monkeylist with
    | [] -> (max1, max2)
    | monkey :: rest ->
        if monkey.inspections_done > max1 then
          loop (monkey.inspections_done, max1) rest
        else if monkey.inspections_done > max2 then
          loop (max1, monkey.inspections_done) rest
        else loop (max1, max2) rest
  in
  let max1, max2 = loop (0, 0) (Map.data individuals) in
  max1 * max2
