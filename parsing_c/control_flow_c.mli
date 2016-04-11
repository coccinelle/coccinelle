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
                 (declOrExpr * exprStatement wrap * exprStatement wrap)
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

  (* IfdefIteHeader is the header node for Ifdef_Ite selection statements.
   * Ifdef_Ite is decorated on top of the CFG for an If statement.
   *)
  | IfdefIteHeader of il

  (* ------------------------ *)
  | DefineHeader of string wrap * define_kind

  | DefineExpr of expression
  | DefineType of fullType
  | DefineDoWhileZeroHeader of unit wrap

  | DefineTodo

  | Include of includ

  | PragmaHeader of string wrap * pragmainfo

  | MacroTop of string * argument wrap2 list * il

  (* ------------------------ *)
  | Case      of statement * expression wrap
  | Default   of statement * unit wrap

  | Continue  of statement * unit wrap
  | Break     of statement * unit wrap * bool (* true for switch *)

  (* no counter part in cocci *)
  | CaseRange of statement * (expression * expression) wrap
  | Label     of statement * name * unit wrap
  | Goto      of statement * name * unit wrap


  | Asm of statement * asmbody wrap
  | MacroStmt of statement * unit wrap

  | Exec of statement * exec_code list wrap

  (* ------------------------ *)
  | Enter
  | Exit
  | Fake
  | CaseNode of int

  (* ------------------------ *)
  (* for ctl:  *)
  | TrueNode of bool ref
  | FalseNode
  | InLoopNode
  | AfterNode of after_type
  | FallThroughNode
  | LoopFallThroughNode
  | ErrorExit

and after_type =
  | RetAfterNode (* after for a block ending in return *)
  | GotoAfterNode (* after for a block ending in goto *)
  | BreakAfterNode (* after for a block ending in break *)
  | ContAfterNode (* after for a block ending in continue *)
  | SWBreakAfterNode (* after for a block ending in break from switch *)
  | NormalAfterNode

type edge = Direct

module Key : Set.OrderedType with type t = int
module KeySet : Set.S with type elt = Key.t
module KeyMap : Map.S with type key = Key.t
module Edge : Set.OrderedType with type t = edge
module KeyEdgePair : Set.OrderedType with type t = Key.t * Edge.t
module KeyEdgeSet : Set.S with type elt = KeyEdgePair.t
module G : Ograph_extended.S with
  type key = Key.t and
  type 'a keymap = 'a KeyMap.t and
  type edge = edge and
  type edges = KeyEdgeSet.t

type cflow = node G.ograph_mutable

val unwrap : node -> node2
val rewrap : node -> node2 -> node

val extract_labels : node -> int list
val extract_bclabels : node -> int list
val extract_fullstatement : node -> Ast_c.statement option
val extract_is_loop : node -> bool
val extract_is_fake : node -> bool

val mk_node: node2 -> int list -> int list -> string -> node
val mk_fake_node: node2 -> int list -> int list -> string -> node

val first_node : cflow -> G.key
val find_node : (node2 -> bool) -> cflow -> G.key
(* remove an intermediate node and redirect the connection  *)
val remove_one_node : G.key -> cflow -> unit
