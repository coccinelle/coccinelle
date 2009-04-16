open Ast_c

type node = node1 * string (* For debugging. Used by print_graph *)
  and node1 = node2 * nodeinfo 
   and nodeinfo = {
      labels: int list;   (* Labels. Trick used for CTL engine *)
      bclabels: int list; (* parent of a break or continue node *)
      is_loop: bool;
      is_fake: bool;
    }
  and node2 =
  | TopNode 
  | EndNode

  | FunHeader     of definition (* but empty body *)
  | Decl          of declaration

  | SeqStart      of statement * int * info
  | SeqEnd        of int * info

  | ExprStatement of statement * (expression option) wrap

  | IfHeader      of statement * expression wrap
  | Else          of info
  | WhileHeader   of statement * expression wrap
  | DoHeader      of statement * info
  | DoWhileTail   of expression wrap
  | ForHeader     of statement * 
                 (exprStatement wrap * exprStatement wrap * exprStatement wrap)
                 wrap
  | SwitchHeader  of statement * expression wrap
  | MacroIterHeader of statement * (string * argument wrap2 list) wrap

  | EndStatement  of info option

  | Return        of statement * unit wrap
  | ReturnExpr    of statement * expression wrap


  (* ------------------------ *)
  | IfdefHeader of ifdef_directive
  | IfdefElse of ifdef_directive
  | IfdefEndif of ifdef_directive

  (* ------------------------ *)
  | DefineHeader of string wrap * define_kind

  | DefineExpr of expression
  | DefineType of fullType
  | DefineDoWhileZeroHeader of unit wrap

  | DefineTodo

  | Include of includ

  | MacroTop of string * argument wrap2 list * il 

  (* ------------------------ *)
  | Case      of statement * expression wrap
  | Default   of statement * unit wrap

  | Continue  of statement * unit wrap
  | Break     of statement * unit wrap

  (* no counter part in cocci *)
  | CaseRange of statement * (expression * expression) wrap
  | Label     of statement * name * unit wrap
  | Goto      of statement * name * unit wrap


  | Asm of statement * asmbody wrap
  | MacroStmt of statement * unit wrap


  (* ------------------------ *)
  | Enter 
  | Exit
  | Fake
  | CaseNode of int 

  (* ------------------------ *)
  (* for ctl:  *)
  | TrueNode
  | FalseNode
  | InLoopNode
  | AfterNode
  | FallThroughNode
  | ErrorExit

type edge = Direct

type cflow = (node, edge) Ograph_extended.ograph_mutable

val unwrap : node -> node2
val rewrap : node -> node2 -> node

val extract_labels : node -> int list
val extract_bclabels : node -> int list
val extract_fullstatement : node -> Ast_c.statement option
val extract_is_loop : node -> bool
val extract_is_fake : node -> bool

val mk_node: node2 -> int list -> int list -> string -> node
val mk_fake_node: node2 -> int list -> int list -> string -> node

val first_node : cflow -> Ograph_extended.nodei
val find_node : (node2 -> bool) -> cflow -> Ograph_extended.nodei
(* remove an intermediate node and redirect the connexion  *)
val remove_one_node : Ograph_extended.nodei -> cflow -> unit
