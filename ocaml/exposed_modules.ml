(* Modules accessible by the ocaml scripts.
 *)

module Ast_c    = Ast_c      (* parsing_c/ast_c.ml *)
module Parse_c  = Parse_c    (* parsing_c/parse_c.ml *)
module Parser_c = Parser_c   (* parsing_c/parser_c.mly *)
module Lexer_c  = Lexer_c    (* parsing_c/lexer_c.mll *)
module Pretty_print_c = Pretty_print_c (* parsing_c/pretty_print_c.ml *)
module Lib_parsing_c  = Lib_parsing_c  (* parsing_c/lib_parsing_c.ml *)
module Visitor_c      = Visitor_c      (* parsing_c/visitor_c.ml *)

module Regexp     = Regexp     (* globals/regexp.ml *)
module Config     = Config     (* globals/config.ml *)
module Flag       = Flag       (* globals/flag.ml *)
module Flag_parsing_c = Flag_parsing_c       (* parsing_c/flag_parsing_c.ml *)
module Iteration  = Iteration  (* globals/iteration.ml *)
module Commands   = Commands   (* commons/commands.ml *)
module Common     = Common     (* commons/common.ml *)

module Ast_cocci  = Ast_cocci  (* parsing_cocci/ast_cocci.ml *)
module Ast0_cocci = Ast0_cocci (* parsing_cocci/ast0_cocci.ml *)

module Dumper = Dumper (* commons/ocamlextra/dumper.ml *)
