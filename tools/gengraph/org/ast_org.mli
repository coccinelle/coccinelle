type orgoption =
    Line of int
  | ColB of int
  | ColE of int
  | Face

type pos = int * int * int
type path = string
type text = string * int
type link = path * orgoption list * text

type status =
    TODO
  | OK
  | BUG
  | FP
  | Unknow of string

type org = status * link
type orgs = org list

type bug = status * path * int * pos * string * next
and next = {
    mutable def: bug option
}

type bugs = bug list
