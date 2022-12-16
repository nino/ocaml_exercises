open Base
open Stdio

let day = 7

module Fs : sig
  type t
  type dir

  val sexp_of_t : t -> Sexp.t
  val create_root : unit -> t
  val execute : fs:t -> cwd:string Stack.t -> string list list -> unit
  val all_directories : t -> dir Sequence.t
  val dir_size : dir -> int
  val print_tree : ?indent:int -> t -> unit
  val used_disk_space : t -> int
end = struct
  type dir = (string, t) Hashtbl.t
  and t = File of int | Dir of dir

  let rec sexp_of_t t =
    match t with
    | File f -> Int.sexp_of_t f
    | Dir dir -> List [ Hashtbl.sexp_of_t String.sexp_of_t sexp_of_t dir ]

  let create_dir () = Dir (Hashtbl.create (module String))

  let create_root () =
    let dir = Hashtbl.create (module String) in
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

  let used_disk_space t =
    match t with 
    | File size -> size
    | Dir dir -> dir_size dir

  let rec print_tree ?(indent = 0) fs =
    let spaces = String.init (indent * 4) ~f:(fun _ -> ' ') in
    match fs with
    | File _ -> printf "%sThis is just a file.\n" spaces
    | Dir dir ->
        Hashtbl.iteri dir ~f:(fun ~key ~data ->
            match data with
            | Dir subdir ->
                printf "%s%s (%d):\n" spaces key (dir_size subdir);
                print_tree ~indent:(indent + 1) data
            | File size -> printf "%s%s, %d\n" spaces key size)

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
        | [ "$"; "cd"; "/" ] -> ()
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

let tests () =
  let input =
    [
      "$ cd /";
      "$ ls";
      "dir subdir2";
      "dir subdir";
      "100 my_file";
      "200 other_file";
      "$ cd subdir";
      "$ ls";
      "200 third_file";
      "$ cd ..";
      "$ cd subdir2";
      "$ ls";
      "50 file5";
      "dir subsubdir";
      "$ cd subsubdir";
      "$ ls";
      "2 small";
    ]
  in

  let commands =
    List.group input ~break:(fun _ line2 -> String.is_prefix line2 ~prefix:"$ ")
  in
  let fs = Fs.create_root () in
  let cwd = Stack.create () in
  Fs.execute ~fs ~cwd commands;
  Fs.print_tree fs;
  printf "%s.\n" (Fs.sexp_of_t fs |> Sexp.to_string_hum ~indent:4);
  Sequence.iter (Fs.all_directories fs) ~f:(fun dir ->
      printf "The dir is %d big.\n" (Fs.dir_size dir));
  ()

let run () =
  tests ();
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

  let total_disk_size = 70000000 in
  let required_free_space = 30000000 in
  let used_disk_space = Fs.used_disk_space fs in
  let actual_free_space = total_disk_size - used_disk_space in
  let what_we_need_to_delete = required_free_space - actual_free_space in
  printf "we need to delete %d.\n" what_we_need_to_delete;
  let smallest_directory_thats_big_enough =
    Fs.all_directories fs
    |> Sequence.map ~f:Fs.dir_size
    |> Sequence.filter ~f:(fun size -> size >= what_we_need_to_delete)
    |> Sequence.min_elt ~compare:Int.compare
  in
  match smallest_directory_thats_big_enough with
  | Some num ->
      printf "The smallest directory big enough to be enough is %d big.\n" num
  | None ->
      printf "No directory is big enough.\n";

      ()
