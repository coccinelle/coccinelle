open Bdb

open Oassoc

open Common
let transact2() = None

(* !!take care!!: this class does side effect, not a pure oassoc *)
class ['a,'b] oassocbtree xs db transact (*fkey unkey*) fv unv = 
  object(o)
    inherit ['a,'b] oassoc

    val data = db

    method empty = raise Todo
    method add (k,v) = 
(*      let _ = pr2 (fkey k) in *)
(*      let _ = pr2 (debugv v) in *)
      (*((try Db.del data None (Marshal.to_string k []) [] with Not_found -> ());*)
      (Db.put data (transact()) (Marshal.to_string k []) (Marshal.to_string (fv v) [Marshal.Closures]) []; o)
    method iter f = 
      let dbc = Cursor.db_cursor db (transact()) [] in
      let rec aux dbc = 
	try (
	  let a = Cursor.dbc_get dbc [Cursor.DB_NEXT] in
	  let key  = Marshal.from_string (fst a) 0 in (* unkey de not marshall *)
	  let valu = unv (Marshal.from_string (snd a) 0) in
	  (f (key, valu);
	   aux dbc)
	 ) with Failure "ending" -> ()  (* todo? may not be tail called cos of the try ?*)
      in begin aux dbc; Cursor.dbc_close dbc end

    method view = raise Todo

    method del (k,v) = raise Todo
    method mem e = raise Todo
    method null = raise Todo

    method assoc k = 
      try 
        unv (Marshal.from_string (Db.get data (transact()) (Marshal.to_string k []) []) 0)
      with Not_found -> (log ("pb assoc with k = " ^ (Dumper.dump k)); raise Not_found) 

    method delkey k = (Db.del data (transact()) (Marshal.to_string k []) []; o)
end	
