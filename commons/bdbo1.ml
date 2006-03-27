open Bdb

open Oassoc

open Common
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
      (Db.put data ~txn:(transact()) ~key:(Marshal.to_string k []) ~data:(Marshal.to_string (fv v) [Marshal.Closures]) []; o)
    method iter f = 
      let dbc = Cursor.create ~writecursor:false ~txn:(transact()) db in
      let rec aux dbc = 
	try (
	  let a = Cursor.get dbc Cursor.NEXT [] in
	  let key  = Marshal.from_string (fst a) 0 in (* unkey de not marshall *)
	  let valu = unv (Marshal.from_string (snd a) 0) in
	  (f (key, valu);
	   aux dbc)
	 ) with _ (*Failure "ending"*) -> ()  (* todo? may not be tail called cos of the try ?*)
      in begin aux dbc; Cursor.close dbc end

    method view = raise Todo

    method del (k,v) = raise Todo
    method mem e = raise Todo
    method null = raise Todo

    method assoc k = unv (Marshal.from_string (Db.get data ~txn:(transact()) (Marshal.to_string k []) []) 0)
    method delkey k = (Db.del data ~txn:(transact()) (Marshal.to_string k []) ; o)
end	
