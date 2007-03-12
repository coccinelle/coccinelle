module Ast0 = Ast0_cocci

(* Things that need to be seen by the lexer and parser. *)

let in_meta = ref false
let in_iso = ref false

type line_type =
    MINUS | OPTMINUS | UNIQUEMINUS | MULTIMINUS
  | PLUS
  | CONTEXT | UNIQUE | OPT | MULTI

let clear_meta: (unit -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_id_meta: (string -> Ast0.pure -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_text_meta: (string -> Ast0.pure -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_type_meta: (string -> Ast0.pure -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_param_meta: (string -> Ast0.pure -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_paramlist_meta: (string -> Ast0.pure -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_const_meta:
    (Type_cocci.typeC list option -> string -> Ast0.pure -> unit) ref =
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_err_meta: (string -> Ast0.pure -> unit) ref =
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_exp_meta:
    (Type_cocci.typeC list option -> string -> Ast0.pure -> unit) ref =
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_explist_meta: (string -> Ast0.pure -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_stm_meta: (string -> Ast0.pure -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_stmlist_meta: (string -> Ast0.pure -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_func_meta: (string -> Ast0.pure -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_local_func_meta: (string -> Ast0.pure -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_meta") 

let add_type_name: (string -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_type") 

let add_declarer_name: (string -> unit) ref = 
  ref (fun _ -> failwith "uninitialized add_decl") 

(* ---------------------------------------------------------------------- *)

(* types that clutter the .mly file *)
(* for iso metavariables, true if they can only match nonmodified, unitary
   metavariables *)
type fresh = bool

type clt = line_type * int * int * int
