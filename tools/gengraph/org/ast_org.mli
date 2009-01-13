type option =
    Line of int
  | ColB of int
  | ColE of int
  | Face

type path = string
type text = string * int
type link = path * option list * text

type status =
    TODO
  | OK
  | BUG
  | FP
  | Unknow of string

type org = status * link
type orgs = org list
