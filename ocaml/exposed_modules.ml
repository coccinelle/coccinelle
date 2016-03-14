(* Modules accessible by the ocaml scripts.
 *)

(*
 We use
 module Foo = struct include Bar end
 rather than
 module Foo = Bar
 Because of an incompatible change in the interpretation of
 module aliases introduced in OCaml 4.02.1.
 See http://caml.inria.fr/mantis/view.php?id=6661
*)

module Ast_c    = struct include Ast_c end
module Parse_c  = struct include Parse_c end
module Parser_c = struct include Parser_c end
module Lexer_c  = struct include Lexer_c end
module Pretty_print_c = struct include Pretty_print_c end
module Lib_parsing_c  = struct include Lib_parsing_c end
module Visitor_c      = struct include Visitor_c end

module Regexp     = struct include Regexp end
module Config     = struct include Config end
module Flag       = struct include Flag end
module Iteration  = struct include Iteration end
module Common     = struct include Common end

module Ast_cocci  = struct include Ast_cocci end
module Ast0_cocci = struct include Ast0_cocci end
module Type_cocci = struct include Type_cocci end

module Dumper = struct include Dumper end
