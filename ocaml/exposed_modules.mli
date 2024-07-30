(* Modules accessible by the ocaml scripts.
 *)

module Token_c  : module type of struct include Token_c end
    (* parsing_c/token_c.ml *)
module Ast_c    : module type of struct include Ast_c end
    (* parsing_c/ast_c.ml *)
module Parse_c  : module type of struct include Parse_c end
    (* parsing_c/parse_c.ml *)
module Parser_c : module type of struct include Parser_c end
    (* parsing_c/parser_c.mly *)
module Lexer_c  : module type of struct include Lexer_c end
    (* parsing_c/lexer_c.mll *)
module Pretty_print_c : module type of struct include Pretty_print_c end
    (* parsing_c/pretty_print_c.ml *)
module Lib_parsing_c  : module type of struct include Lib_parsing_c end
    (* parsing_c/lib_parsing_c.ml *)
module Visitor_c      : module type of struct include Visitor_c end
    (* parsing_c/visitor_c.ml *)

module Regexp     : module type of struct include Regexp end
    (* globals/regexp.ml *)
module Cocciconfig     : module type of struct include Cocciconfig end
    (* globals/cocciconfig.ml *)
module Flag       : module type of struct include Flag end
    (* globals/flag.ml *)
module Flag_parsing_c : module type of struct include Flag_parsing_c end
    (* parsing_c/flag_parsing_c.ml *)
module Iteration  : module type of struct include Iteration end
    (* globals/iteration.ml *)
module Commands   : module type of struct include Commands end
    (* commons/commands.ml *)
module Common     : module type of struct include Common end
    (* commons/common.ml *)

module Ast_cocci  : module type of struct include Ast_cocci end
    (* parsing_cocci/ast_cocci.ml *)
module Ast0_cocci : module type of struct include Ast0_cocci end
    (* parsing_cocci/ast0_cocci.ml *)

module Dumper : module type of struct include Dumper end
    (* commons/ocamlextra/dumper.ml *)
