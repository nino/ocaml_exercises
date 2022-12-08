open Base
open Stdio

let day = 7

module Fs : sig
  type t
  type dir

  val create_root : unit -> t
  val execute : fs:t -> cwd:string Stack.t -> string list list -> unit
  val all_directories : t -> dir Sequence.t
  val dir_size : dir -> int
end = struct
  type dir = (string, t) Hashtbl.t
  and t = File of int | Dir of dir

  let create_dir () = Dir (Hashtbl.create (module String))

  let create_root () =
    let dir = Hashtbl.create (module String) in
    Hashtbl.set dir ~key:"/" ~data:(create_dir ());
    Dir dir

  let entry_of_string (string : string) : string * t =
    match String.split ~on:' ' string with
    | [ "dir"; name ] -> (name, create_dir ())
    | [ size; name ] -> (name, File (Int.of_string size))
    | _ -> failwith ("Invalid dir entry " ^ string)

  let rec all_directories fs =
    match fs with
    | File _ -> Sequence.init 0 ~f:(fun _ -> Hashtbl.create (module String))
    | Dir dir ->
        let subdirs = Sequence.of_list (Hashtbl.data dir) in
        Sequence.append (Sequence.return dir)
          (Sequence.concat_map subdirs ~f:all_directories)

  let rec dir_size dir =
    let entries = Hashtbl.data dir in
    List.fold entries ~init:0 ~f:(fun sum entry ->
        match entry with
        | File size -> sum + size
        | Dir subdir -> sum + dir_size subdir)

  let rec add_entry fs ~path input =
    let name, entry = input in
    match fs with
    | File _ ->
        failwith
          ("Cannot add entry with name " ^ name
         ^ " because the path is not a directory.")
    | Dir dir -> (
        match path with
        | [] -> Hashtbl.set dir ~key:name ~data:entry
        | hd :: tl -> add_entry (Hashtbl.find_exn dir hd) ~path:tl input)

  let execute_single ~fs ~cwd lines =
    match lines with
    | cmd :: output -> (
        match String.split ~on:' ' cmd with
        | [ "$"; "cd"; ".." ] -> ignore (Stack.pop cwd)
        | [ "$"; "cd"; dir ] -> Stack.push cwd dir
        | [ "$"; "ls" ] ->
            List.iter output ~f:(fun line ->
                add_entry fs
                  ~path:(List.rev @@ Stack.to_list cwd)
                  (entry_of_string line))
        | _ -> failwith ("Invalid command " ^ cmd))
    | [] -> failwith "For some reason this cmd is empty"

  let execute ~fs ~cwd commands =
    List.iter commands ~f:(fun lines -> execute_single ~fs ~cwd lines)
end

let run () =
  let input = In_channel.read_lines (Utils.input_path_for_day 7) in
  let commands =
    List.group input ~break:(fun _ line2 -> String.is_prefix line2 ~prefix:"$ ")
  in
  let fs = Fs.create_root () in
  let cwd = Stack.create () in
  Fs.execute ~fs ~cwd commands;
  let all_directories = Fs.all_directories fs in
  let total_size_of_small_dirs =
    Sequence.fold all_directories ~init:0 ~f:(fun sum dir ->
        let size = Fs.dir_size dir in
        if size <= 100_000 then sum + size else sum)
  in
  printf "The total size of all directories under size 100_000 is %d.\n"
    total_size_of_small_dirs;

  ()
