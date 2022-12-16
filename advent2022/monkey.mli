open Base

type t
type monkeys
type operation

val parse_monkeys : string -> (monkeys, Error.t) Result.t
val iterate_times : ?panic:bool -> int -> monkeys -> monkeys
val monkey_business : monkeys -> int
