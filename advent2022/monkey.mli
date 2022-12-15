open Base

type t
type monkeys
type operation

val parse_monkeys : string -> (monkeys, Error.t) Result.t
val do_op : operation -> int -> int
val iterate : monkeys -> monkeys
val monkey_business : monkeys -> int
