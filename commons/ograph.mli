class virtual ['a] ograph :
  object ('b)
    method virtual add_arc : 'a * 'a -> 'b
    method virtual add_node : 'a -> 'b
    method virtual ancestors : 'a Oset.oset -> 'a Oset.oset
    method virtual brothers : 'a -> 'a Oset.oset
    method virtual children : 'a Oset.oset -> 'a Oset.oset
    method debug : ('a * 'a list) list
    method virtual del_arc : 'a * 'a -> 'b
    method virtual del_node : 'a -> 'b
    method virtual empty : 'b
    method virtual nodes : 'a Oset.oset
    method virtual predecessors : 'a -> 'a Oset.oset
    method virtual successors : 'a -> 'a Oset.oset
  end
