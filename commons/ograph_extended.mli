type nodei = int

class ['a, 'b] ograph_extended :
  object ('c)
    method add_arc : (nodei * nodei) * 'b -> 'c
    method add_node : 'a -> 'c * nodei
    method add_nodei : nodei -> 'a -> 'c * nodei
    method allsuccessors : (nodei, (nodei * 'b) Osetb.osetb) Oassocb.oassocb
    method del_arc : (nodei * nodei) * 'b -> 'c
    method del_node : nodei * 'a -> 'c
    method nodes : (nodei, 'a) Oassocb.oassocb
    method predecessors : nodei -> (nodei * 'b) Osetb.osetb
    method replace_node : nodei * 'a -> 'c
    method successors : nodei -> (nodei * 'b) Osetb.osetb
  end

val print_ograph_extended : ('a * string, 'b) ograph_extended -> unit
