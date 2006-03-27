open Common

open Oassoc

(* !!take care!!: this class does side effect, not a pure oassoc *)
class ['a,'b] oassocdbm xs db (*fkey unkey*) fv unv = 
  object(o)
    inherit ['a,'b] oassoc

    val db = db

    method empty = raise Todo
    method add (k,v) = 
(*      let _ = pr2 (fkey k) in *)
(*      let _ = pr2 (debugv v) in *)
      (*((try Db.del data None (Marshal.to_string k []) [] with Not_found -> ());*)
      ((try Dbm.add db (Marshal.to_string k []) (Marshal.to_string (fv v) [Marshal.Closures])
          with _ -> Dbm.replace db (Marshal.to_string k []) (Marshal.to_string (fv v) [Marshal.Closures]))
         ;
       o)
    method iter f = 
      Dbm.iter (fun key data -> 
	let key  = Marshal.from_string key 0 in (* unkey de not marshall *)
	let valu = unv (Marshal.from_string data 0) in
	f (key, valu)
	       ) db

    method view = raise Todo

    method del (k,v) = raise Todo
    method mem e = raise Todo
    method null = raise Todo

    method assoc k = unv (Marshal.from_string (Dbm.find db (Marshal.to_string k [])) 0)
    method delkey k = try (Dbm.remove db (Marshal.to_string k []); o) with Dbm.Dbm_error "dbm_delete" -> raise Not_found
end	
