open Common

open Ocollection
open Oset
open Oassoc
(* open Ograph *)

open Oassocb
open Osetb


(* Difference with ograph_extended ? why not share code ? could, but
 * in ograph_extended we dont force the user to have a key and we
 * generate those keys as he add nodes. Here we assume the user already
 * have an idea of what kind of key he wants to use (a string, a
 * filename, a, int, whatever) 
*)

class ['key, 'a,'b] ograph_mutable =
  let build_assoc () = new oassocb [] in
  let build_set ()   = new osetb Setb.empty in

object(o)
  

  val mutable succ = build_assoc()
  val mutable pred = build_assoc()
  val mutable nods = (build_assoc() : ('key, 'a) Oassocb.oassocb)

  method add_node i (e: 'a) = 
    nods <- nods#add (i, e); 
    pred <- pred#add (i, build_set() );
    succ <- succ#add (i, build_set() );

  method del_node (i) = 
    (* check: e is effectively the index associated with e, 
       and check that already in *)

        (* todo: assert that have no pred and succ, otherwise
         * will have some dangling pointers 
         *)
        nods <- nods#delkey i; 
        pred <- pred#delkey i;
        succ <- succ#delkey i;

    method replace_node i (e: 'a) = 
      assert (nods#haskey i);
      nods <- nods#replkey (i, e);
        
    method add_arc ((a,b),(v: 'b)) = 
      succ <- succ#replkey (a, (succ#find a)#add (b, v));
      pred <- pred#replkey (b, (pred#find b)#add (a, v));
    method del_arc ((a,b),v) =
      succ <- succ#replkey (a, (succ#find a)#del (b,v));
      pred <- pred#replkey (b, (pred#find b)#del (a,v));

    method successors   e = succ#find e
    method predecessors e = pred#find e

    method nodes = nods
    method allsuccessors = succ

  end   
