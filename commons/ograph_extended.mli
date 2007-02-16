type nodei = int

class ['a, 'b] ograph_extended :
  object ('c)
    method add_node : 'a -> 'c * nodei
    method add_nodei : nodei -> 'a -> 'c * nodei
    method replace_node : nodei * 'a -> 'c
    method del_node : nodei -> 'c

    method add_arc : (nodei * nodei) * 'b -> 'c
    method del_arc : (nodei * nodei) * 'b -> 'c

    method nodes : (nodei, 'a) Oassocb.oassocb

    method successors : nodei -> (nodei * 'b) Osetb.osetb
    method predecessors : nodei -> (nodei * 'b) Osetb.osetb
    method allsuccessors : (nodei, (nodei * 'b) Osetb.osetb) Oassocb.oassocb
  end

val print_ograph_extended : string -> ('a * string, 'b) ograph_extended -> unit
