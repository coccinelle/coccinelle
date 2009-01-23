
type pos = int * int
type hunk = pos * pos
type path = string

type diff = (int * path) * hunk list
type diffs = diff list
