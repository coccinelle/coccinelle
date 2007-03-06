open Common
open Commonop

(* Tricks used to handle the ambiguity in the grammar with the typedef
 * which impose a cooperation between the lexer and the parser.
 * 
 * An example by hughes casse: NOTE: in the symbol table, local
 * definition must replace type definition in order to correctly parse
 * local variable in functions body. This is the only way to correctly
 * handle this kind of exception, that is,
 * 
 * typedef ... ID; int f(int *p) {int ID; return (ID) * *p;} If ID
 * isn't overload, last expression is parsed as a type cast, if it
 * isn't, this a multiplication.
 * 
 * Why parse_typedef_fix2 ? Cos when introduce new variable (for
 * instance when declare parameters for a function such as int var_t),
 * then the var_t must not be lexed as a typedef, so we must disable
 * temporaly the typedef mechanism to allow variable with same name as
 * a typedef. *)

(* parse_typedef_fix *)
let _handle_typedef = ref true

(* parse_typedef_fix2 *)
let enable_typedef ()  = _handle_typedef := true
let disable_typedef () = _handle_typedef := false


let is_enable_state () = !_handle_typedef


type typedef = TypeDefI of string | IdentI of string

(* oldsimple:  but slow,  take 2 secondes on some C files 
 * let (typedef: typedef list list ref) = ref [[]]
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

(* Ca marche ce code ? on peut avoir un typedef puis un ident puis
 * un typedef nested ? oui car hashtbl gere l'historique. 
 *)
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
let add_typedef_root s = 
  if !Flag_parsing_c.add_typedef_root
  then 
    Hashtbl.add !typedef s (TypeDefI s)
  else add_typedef s (* have far more .failed without this *)
  
let add_ident s    = 
  begin
    scoped_typedef := 
      (IdentI   s::(List.hd !scoped_typedef))::(List.tl !scoped_typedef);
    Hashtbl.add !typedef s (IdentI s);
  end


type lexer_hint = { 
    mutable parameterDeclaration: bool;
    mutable structDefinition: int; (* depth in struct def, 0 = not in struct *)
(*    mutable statements: bool; *)
    mutable toplevel: bool;
    mutable define: bool;
  }

let default_hint () = { 
  parameterDeclaration = false;
  (* statements = false; *)
  structDefinition = 0;
  toplevel = false;
  define = false;
}

let _lexer_hint = ref (default_hint())


let lexer_reset_typedef () = 
  begin
  _handle_typedef := true;
  typedef := Hashtbl.create 100;
  scoped_typedef := [[]];
  _lexer_hint := { (default_hint ()) with toplevel = true; } ;
  end

