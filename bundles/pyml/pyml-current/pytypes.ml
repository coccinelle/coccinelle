type pyobject

type compare = LT | LE | EQ | NE | GT | GE

type input = Single | File | Eval

let int_of_compare c =
  match c with
    LT -> 0
  | LE -> 1
  | EQ -> 2
  | NE -> 3
  | GT -> 4
  | GE -> 5

let compare_of_int c =
  match c with
    0 -> LT
  | 1 -> LE
  | 2 -> EQ
  | 3 -> NE
  | 4 -> GT
  | 5 -> GE
  | _ -> failwith "Pytypes.compare_of_int"
