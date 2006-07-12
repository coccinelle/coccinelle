open Common
open Commonop

(* Tricks used to handle the ambiguity in the grammar with the typedef which
   impose a cooperation between the lexer and the parser.

   An example by hughes casse:
      NOTE: in the symbol table, local definition must replace type definition
	    in order to correctly parse local variable in functions body.
	    This is the only way to correctly handle this kind of exception,
	    that is,

	    typedef ... ID;
	    int f(int *p) {int ID; return (ID) * *p;}
	    If ID isn't overload, last expression is parsed as a type cast,
	    if it isn't, this a multiplication.


  Why parse_typedef_fix2 ? 
   Cos when introduce new variable (for instance when declare parameters 
   for a function such as int var_t), then the var_t must not be lexed as a 
   typedef,  so we must disable temporaly the typedef mechanism to allow
   variable with same name as a typedef.
*)

(*  parse_typedef_fix *)
let _handle_typedef = ref true

(*  parse_typedef_fix2 *)
let enable_typedef ()  = _handle_typedef := true
let disable_typedef () = _handle_typedef := false
(*  let enable_typedef ()  = () *)
(*  let disable_typedef () = () *)



type typedef = TypeDefI of string | IdentI of string

(*
     (*  oldsimple:  but slow,  take 2 secondes on some C files  *)
     let (typedef: typedef list list ref) = ref [[]]
     
     (*  opti?: ajouter ident que si y'avait un typedef deja la  *)
     let is_typedef   s = if !_handle_typedef then 
       (let rec aux = function 
           (* fun x -> Common.count1(); match x with *)
         | [] -> false
         | x::xs -> 
     	let rec aux2 = function 
                 (* fun x -> Common.count2(); match x with *)
     	  | (TypeDefI s2::xs) when s = s2 -> true 
     	  | (IdentI s2::xs)   when s = s2 -> false 
     	  | (y::xs) -> aux2 xs
     	  | [] -> raise Not_found
     	in try aux2 x with Not_found -> aux xs
       in aux !typedef
       ) else false
     
     let new_scope() = typedef := []::!typedef
     let del_scope() = typedef := List.tl !typedef
     
     let add_typedef  s = 
       typedef := (TypeDefI s::(List.hd !typedef))::(List.tl !typedef)
     let add_ident s    = 
       typedef := (IdentI   s::(List.hd !typedef))::(List.tl !typedef)
*)





let (typedef: ((string, typedef) Hashtbl.t) ref) =  ref (Hashtbl.create 100)
let (scoped_typedef: typedef list list ref) = ref [[]]

let is_typedef s  = if !_handle_typedef then
  (match (Common.optionise (fun () -> Hashtbl.find !typedef s)) with
  | Some (TypeDefI s2) -> assert (s = s2); true
  | Some (IdentI s2) ->   assert (s = s2); false
  | None -> false
  )
  else false

let new_scope() = scoped_typedef := []::!scoped_typedef
let del_scope() = 
  begin
    List.hd !scoped_typedef +> List.iter (function 
      | ((TypeDefI s)|(IdentI s)) -> Hashtbl.remove !typedef s);
    scoped_typedef := List.tl !scoped_typedef
  end

let add_typedef  s = 
  begin 
    scoped_typedef := 
      (TypeDefI s::(List.hd !scoped_typedef))::(List.tl !scoped_typedef);
    Hashtbl.add !typedef s (TypeDefI s);
  end
let add_ident s    = 
  begin
    scoped_typedef := 
      (IdentI   s::(List.hd !scoped_typedef))::(List.tl !scoped_typedef);
    Hashtbl.add !typedef s (IdentI s);
  end


type lexer_hint = 
  | ParameterDeclaration 
  | StructDefinition
  | Statements
  | Toplevel

let _lexer_hint = ref (None: lexer_hint option)


let lexer_reset_typedef () = 
  begin
  _handle_typedef := true;
  typedef := Hashtbl.create 100;
  scoped_typedef := [[]];
  _lexer_hint := Some Toplevel;
  end

