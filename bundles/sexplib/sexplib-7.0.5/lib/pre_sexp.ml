(******************************************************************************
 *                             Sexplib                                        *
 *                                                                            *
 * Copyright (C) 2005- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *    Author: Markus Mottl                                                    *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

(* Sexp: Module for handling S-expressions (I/O, etc.) *)

open Format
open Bigarray

include Type

exception Of_sexp_error of exn * t

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t


(* Default indentation level for human-readable conversions *)

let default_indent = ref 1


(* Escaping of strings used as atoms in S-expressions *)

let is_special_char c =
  c <= ' ' || c = '"' || c = '(' || c = ')' || c = ';' || c = '\\'

let must_escape str =
  let len = String.length str in
  len = 0 ||
    let rec loop ix = is_special_char str.[ix] || ix > 0 && loop (ix - 1) in
    loop (len - 1)

let maybe_esc_str str =
  if must_escape str then
    let estr = String.escaped str in
    let elen = String.length estr in
    let res = String.create (elen + 2) in
    String.blit estr 0 res 1 elen;
    res.[0] <- '"';
    res.[elen + 1] <- '"';
    res
  else str

let pp_maybe_esc_str ppf str = pp_print_string ppf (maybe_esc_str str)


(* Output of S-expressions to formatters *)

let rec pp_hum_indent indent ppf = function
  | Atom str -> pp_maybe_esc_str ppf str
  | List (h :: t) ->
      pp_open_box ppf indent;
      pp_print_string ppf "(";
      pp_hum_indent indent ppf h;
      pp_hum_rest indent ppf t
  | List [] -> pp_print_string ppf "()"

and pp_hum_rest indent ppf = function
  | h :: t ->
      pp_print_space ppf ();
      pp_hum_indent indent ppf h;
      pp_hum_rest indent ppf t
  | [] ->
      pp_print_string ppf ")";
      pp_close_box ppf ()

let rec pp_mach_internal may_need_space ppf = function
  | Atom str ->
      let str' = maybe_esc_str str in
      let new_may_need_space = str' == str in
      if may_need_space && new_may_need_space then pp_print_string ppf " ";
      pp_print_string ppf str';
      new_may_need_space
  | List (h :: t) ->
      pp_print_string ppf "(";
      let may_need_space = pp_mach_internal false ppf h in
      pp_mach_rest may_need_space ppf t;
      false
  | List [] -> pp_print_string ppf "()"; false

and pp_mach_rest may_need_space ppf = function
  | h :: t ->
      let may_need_space = pp_mach_internal may_need_space ppf h in
      pp_mach_rest may_need_space ppf t
  | [] -> pp_print_string ppf ")"

let pp_hum ppf sexp = pp_hum_indent !default_indent ppf sexp

let pp_mach ppf sexp = ignore (pp_mach_internal false ppf sexp)
let pp = pp_mach


(* Sexp size *)

let rec size_loop (v, c as acc) = function
  | Atom str -> v + 1, c + String.length str
  | List lst -> List.fold_left size_loop acc lst

let size sexp = size_loop (0, 0) sexp


(* Buffer conversions *)

let to_buffer_hum ~buf ?(indent = !default_indent) sexp =
  Format.bprintf buf "%a@?" (pp_hum_indent indent) sexp

let to_buffer_mach ~buf sexp =
  let rec loop may_need_space = function
    | Atom str ->
        let str' = maybe_esc_str str in
        let new_may_need_space = str' == str in
        if may_need_space && new_may_need_space then Buffer.add_char buf ' ';
        Buffer.add_string buf str';
        new_may_need_space
    | List (h :: t) ->
        Buffer.add_char buf '(';
        let may_need_space = loop false h in
        loop_rest may_need_space t;
        false
    | List [] -> Buffer.add_string buf "()"; false
  and loop_rest may_need_space = function
    | h :: t ->
        let may_need_space = loop may_need_space h in
        loop_rest may_need_space t
    | [] -> Buffer.add_char buf ')' in
  ignore (loop false sexp)

let to_buffer = to_buffer_mach


(* Output of S-expressions to I/O-channels *)

let buffer () = Buffer.create 4096

let with_new_buffer oc f =
  let buf = buffer () in
  f buf;
  Buffer.output_buffer oc buf

let output_hum oc sexp =
  with_new_buffer oc (fun buf -> to_buffer_hum sexp ~buf)

let output_hum_indent indent oc sexp =
  with_new_buffer oc (fun buf -> to_buffer_hum ~indent sexp ~buf)

let output_mach oc sexp =
  with_new_buffer oc (fun buf -> to_buffer_mach sexp ~buf)

let output = output_mach


(* Output of S-expressions to file *)

let save_of_output ?perm output_function file sexp =
  let tmp_name, oc = Filename.open_temp_file file "tmp" in
  try
    output_function oc sexp;
    close_out oc;
    let perm =
      match perm with
      | Some perm -> perm
      | None ->
          let umask = Unix.umask 0 in
          ignore (Unix.umask umask);
          umask lxor 0o666
    in
    if perm <> 0o600 then Unix.chmod tmp_name perm;
    Sys.rename tmp_name file
  with
    e ->
      close_out_noerr oc;
      raise e

let output_sexp_nl do_output oc sexp =
  do_output oc sexp;
  output_string oc "\n"

let save_hum ?perm file sexp =
  save_of_output ?perm (output_sexp_nl output_hum) file sexp

let save_mach ?perm file sexp = save_of_output ?perm output_mach file sexp
let save = save_mach

let output_sexps_nl do_output oc sexps =
  List.iter (output_sexp_nl do_output oc) sexps

let save_sexps_hum ?perm file sexps =
  save_of_output ?perm (output_sexps_nl output_hum) file sexps

let save_sexps_mach ?perm file sexps =
  save_of_output ?perm (output_sexps_nl output_mach) file sexps

let save_sexps = save_sexps_mach


(* String conversions *)

let to_string_hum ?indent = function
  | Atom str -> maybe_esc_str str
  | sexp ->
      let buf = buffer () in
      to_buffer_hum ?indent sexp ~buf;
      Buffer.contents buf

let to_string_mach = function
  | Atom str -> maybe_esc_str str
  | sexp ->
      let buf = buffer () in
      to_buffer_mach sexp ~buf;
      Buffer.contents buf

let to_string = to_string_mach


(* Scan functions *)

let scan_sexp ?buf lexbuf = Parser.sexp (Lexer.main ?buf) lexbuf
let scan_sexps ?buf lexbuf = Parser.sexps (Lexer.main ?buf) lexbuf

let get_main_buf buf =
  let buf =
    match buf with
    | None -> Buffer.create 64
    | Some buf -> buf in
  Lexer.main ~buf

let scan_fold_sexps ?buf ~f ~init lexbuf =
  let main = get_main_buf buf in
  let rec loop acc =
    match Parser.sexp_opt main lexbuf with
    | None -> acc
    | Some sexp -> loop (f acc sexp) in
  loop init

let scan_iter_sexps ?buf ~f lexbuf =
  scan_fold_sexps ?buf lexbuf ~init:() ~f:(fun () sexp -> f sexp)

let scan_sexps_conv ?buf ~f lexbuf =
  let coll acc sexp = f sexp :: acc in
  List.rev (scan_fold_sexps ?buf ~f:coll ~init:[] lexbuf)


(* Partial parsing *)

module Annot = struct
  type pos = { line : int; col : int; offset : int }
  type range = { start_pos : pos; end_pos : pos }
  type t = Atom of range * Type.t | List of range * t list * Type.t
  type 'a conv = [ `Result of 'a | `Error of exn * t ]

  exception Conv_exn of string * exn

  type stack = {
    mutable positions : pos list;
    mutable stack : t list list;
  }

  let get_sexp = function Atom (_, sexp) | List (_, _, sexp) -> sexp
  let get_range = function Atom (range, _) | List (range, _, _) -> range

  exception Annot_sexp of t

  let find_sexp annot_sexp sexp =
    let rec loop annot_sexp =
      match annot_sexp with
      | Atom (_, sub_sexp)
      | List (_, _, sub_sexp) when sexp == sub_sexp ->
          raise (Annot_sexp annot_sexp)
      | List (_, annots, _) -> List.iter loop annots
      | Atom _ -> ()
    in
    try loop annot_sexp; None
    with Annot_sexp res -> Some res
end

module Parse_pos = struct
  type t =
    {
      mutable text_line : int;
      mutable text_char : int;
      mutable global_offset : int;
      mutable buf_pos : int;
    }

  let create
        ?(text_line = 1) ?(text_char = 0)
        ?(buf_pos = 0) ?(global_offset = 0) () =
    let fail msg = failwith ("Sexplib.Sexp.Parse_pos.create: " ^ msg) in
    if text_line < 1 then fail "text_line < 1"
    else if text_char < 0 then fail "text_char < 0"
    else if global_offset < 0 then fail "global_offset < 0"
    else if buf_pos < 0 then fail "buf_pos < 0"
    else { text_line = text_line; text_char = text_char; global_offset = global_offset; buf_pos = buf_pos }

  let with_buf_pos t buf_pos = { t with buf_pos = buf_pos }
end

type ('a, 't) parse_result =
  | Done of 't * Parse_pos.t
  | Cont of bool * ('a, 't) parse_fun

and ('a, 't) parse_fun = pos : int -> len : int -> 'a -> ('a, 't) parse_result

type 't parse_state =
  {
    parse_pos : Parse_pos.t;
    mutable pstack : 't;
    pbuf : Buffer.t;
  }

type parse_error =
  {
    location : string;
    err_msg : string;
    parse_state :
      [
      | `Sexp of t list list parse_state
      | `Annot of Annot.stack parse_state
      ]
  }

exception Parse_error of parse_error

let bump_text_line { parse_pos = parse_pos } =
  parse_pos.Parse_pos.text_line <- parse_pos.Parse_pos.text_line + 1;
  parse_pos.Parse_pos.text_char <- 0

let bump_text_pos { parse_pos = parse_pos } =
  parse_pos.Parse_pos.text_char <- parse_pos.Parse_pos.text_char + 1

let bump_pos_cont state str ~max_pos ~pos cont =
  bump_text_pos state;
  cont state str ~max_pos ~pos:(pos + 1)

let bump_line_cont state str ~max_pos ~pos cont =
  bump_text_line state;
  cont state str ~max_pos ~pos:(pos + 1)

let add_bump bump state str ~max_pos ~pos c cont =
  Buffer.add_char state.pbuf c;
  bump state;
  cont state str ~max_pos ~pos:(pos + 1)

let add_bump_pos state str ~max_pos ~pos c cont =
  add_bump bump_text_pos state str ~max_pos ~pos c cont

let add_bump_line state str ~max_pos ~pos c cont =
  add_bump bump_text_line state str ~max_pos ~pos c cont

let set_parse_pos parse_pos buf_pos =
  let len = buf_pos - parse_pos.Parse_pos.buf_pos in
  parse_pos.Parse_pos.buf_pos <- buf_pos;
  parse_pos.Parse_pos.global_offset <- parse_pos.Parse_pos.global_offset + len

let mk_parse_pos { parse_pos = parse_pos } buf_pos =
  set_parse_pos parse_pos buf_pos;
  parse_pos

let raise_parse_error parse_state location buf_pos err_msg =
  begin
    match parse_state with
    | `Sexp { parse_pos = parse_pos } | `Annot { parse_pos = parse_pos } ->
        set_parse_pos parse_pos buf_pos;
        parse_pos.Parse_pos.text_char <- parse_pos.Parse_pos.text_char + 1;
  end;
  let parse_error = { location = location; err_msg = err_msg; parse_state = parse_state } in
  raise (Parse_error parse_error)

let raise_unexpected_char parse_state location buf_pos c =
  let err_msg = sprintf "unexpected character: '%c'" c in
  raise_parse_error parse_state location buf_pos err_msg

(* The code below is derived from lexer.mll in the OCaml distribution,
   which also uses ASCII codes instead of escape sequences to denote
   special characters. *)

(* Macro for generating parsers *)
#define MK_PARSER( \
    TYPE, GET_LEN, PARSE, GET_CHAR, \
    GET_PSTACK, SET_PSTACK, \
    REGISTER_POS, REGISTER_POS1, \
    MK_ATOM, MK_LIST, INIT_PSTACK, MK_PARSE_STATE) \
  let bump_found_atom bump state str ~max_pos ~pos cont = \
    let pbuf = state.pbuf in \
    let pbuf_str = Buffer.contents pbuf in \
    let atom = MK_ATOM in \
    match GET_PSTACK with \
    | [] -> Done (atom, mk_parse_pos state pos) \
    | rev_sexp_lst :: sexp_stack -> \
        Buffer.clear pbuf; \
        let pstack = (atom :: rev_sexp_lst) :: sexp_stack in \
        SET_PSTACK; \
        bump state; \
        cont state str ~max_pos ~pos:(pos + 1) \
  \
  let check_str_bounds loc ~pos ~len (str : TYPE) = \
    if pos < 0 then invalid_arg (loc ^ ": pos < 0"); \
    if len < 0 then invalid_arg (loc ^ ": len < 0"); \
    let str_len = GET_LEN str in \
    let pos_len = pos + len in \
    if pos_len > str_len then invalid_arg (loc ^ ": pos + len > str_len"); \
    pos_len - 1 \
  \
  let mk_cont name cont state = \
    let ws_only = GET_PSTACK = [] && Buffer.length state.pbuf = 0 in \
    let parse_fun = \
      let used_ref = ref false in \
      fun ~pos ~len str -> \
        if !used_ref then \
          failwith "Sexplib.Sexp: parser continuation called twice" \
        else begin \
          used_ref := true; \
          let max_pos = check_str_bounds name ~pos ~len str in \
          cont state str ~max_pos ~pos \
        end \
    in \
    Cont (ws_only, parse_fun) \
  \
  let rec PARSE state str ~max_pos ~pos = \
    if pos > max_pos then mk_cont "parse" PARSE state \
    else \
      match GET_CHAR with \
      | '(' -> \
          REGISTER_POS \
          let pstack = [] :: GET_PSTACK in \
          SET_PSTACK; \
          bump_pos_cont state str ~max_pos ~pos PARSE \
      | ')' as c -> \
          (match GET_PSTACK with \
          | [] -> raise_unexpected_char (MK_PARSE_STATE state) "parse" pos c \
          | rev_sexp_lst :: sexp_stack -> \
              let sexp_lst = List.rev rev_sexp_lst in \
              let sexp = MK_LIST in \
              match sexp_stack with \
              | [] -> Done (sexp, mk_parse_pos state (pos + 1)) \
              | higher_rev_sexp_lst :: higher_sexp_stack -> \
                  let pstack = \
                    (sexp :: higher_rev_sexp_lst) :: higher_sexp_stack \
                  in \
                  SET_PSTACK; \
                  bump_pos_cont state str ~max_pos ~pos PARSE) \
      | ' ' | '\009' | '\012' -> bump_pos_cont state str ~max_pos ~pos PARSE \
      | '\010' -> bump_line_cont state str ~max_pos ~pos PARSE \
      | '\013' -> bump_line_cont state str ~max_pos ~pos parse_nl \
      | ';' -> bump_pos_cont state str ~max_pos ~pos parse_comment \
      | '"' -> \
          REGISTER_POS1 \
          bump_pos_cont state str ~max_pos ~pos parse_quoted \
      | c -> \
          REGISTER_POS \
          add_bump_pos state str ~max_pos ~pos c parse_atom \
  \
  and parse_nl state str ~max_pos ~pos = \
    if pos > max_pos then mk_cont "parse_nl" parse_nl state \
    else \
      let pos = if GET_CHAR = '\010' then pos + 1 else pos in \
      PARSE state str ~max_pos ~pos \
  \
  and parse_comment state str ~max_pos ~pos = \
    if pos > max_pos then mk_cont "parse_comment" parse_comment state \
    else \
      match GET_CHAR with \
      | '\010' -> bump_line_cont state str ~max_pos ~pos PARSE \
      | '\013' -> bump_line_cont state str ~max_pos ~pos parse_nl \
      | _ -> bump_pos_cont state str ~max_pos ~pos parse_comment \
  \
  and parse_atom state str ~max_pos ~pos = \
    if pos > max_pos then mk_cont "parse_atom" parse_atom state \
    else \
      match GET_CHAR with \
      | ' ' | '\009' | '\012' -> \
          bump_found_atom bump_text_pos state str ~max_pos ~pos PARSE \
      | '(' -> \
          let pbuf = state.pbuf in \
          let pbuf_str = Buffer.contents pbuf in \
          let atom = MK_ATOM in \
          (match GET_PSTACK with \
          | [] -> Done (atom, mk_parse_pos state pos) \
          | rev_sexp_lst :: sexp_stack -> \
              REGISTER_POS \
              Buffer.clear pbuf; \
              let pstack = [] :: (atom :: rev_sexp_lst) :: sexp_stack in \
              SET_PSTACK; \
              bump_pos_cont state str ~max_pos ~pos PARSE) \
      | ')' -> \
          let pbuf = state.pbuf in \
          let pbuf_str = Buffer.contents pbuf in \
          let atom = MK_ATOM in \
          (match GET_PSTACK with \
          | [] -> Done (atom, mk_parse_pos state pos) \
          | rev_sexp_lst :: sexp_stack -> \
              let sexp_lst = List.rev_append rev_sexp_lst [atom] in \
              let sexp = MK_LIST in \
              match sexp_stack with \
              | [] -> Done (sexp, mk_parse_pos state (pos + 1)) \
              | higher_rev_sexp_lst :: higher_sexp_stack -> \
                  Buffer.clear pbuf; \
                  let pstack = \
                    (sexp :: higher_rev_sexp_lst) :: higher_sexp_stack \
                  in \
                  SET_PSTACK; \
                  bump_pos_cont state str ~max_pos ~pos PARSE) \
      | '\010' -> bump_found_atom bump_text_line state str ~max_pos ~pos PARSE \
      | '\013' -> \
          bump_found_atom bump_text_line state str ~max_pos ~pos parse_nl \
      | ';' -> \
          bump_found_atom bump_text_pos state str ~max_pos ~pos parse_comment \
      | '"' -> \
          bump_found_atom \
            bump_text_pos state str ~max_pos ~pos reg_parse_quoted \
      | c -> add_bump_pos state str ~max_pos ~pos c parse_atom \
  \
  and reg_parse_quoted state str ~max_pos ~pos = \
    REGISTER_POS \
    parse_quoted state str ~max_pos ~pos \
  and parse_quoted state str ~max_pos ~pos = \
    if pos > max_pos then mk_cont "parse_quoted" parse_quoted state \
    else \
      match GET_CHAR with \
      | '"' -> \
          let pbuf = state.pbuf in \
          let pbuf_str = Buffer.contents pbuf in \
          let atom = MK_ATOM in \
          (match GET_PSTACK with \
          | [] -> Done (atom, mk_parse_pos state (pos + 1)) \
          | rev_sexp_lst :: sexp_stack -> \
              Buffer.clear pbuf; \
              let pstack = (atom :: rev_sexp_lst) :: sexp_stack in \
              SET_PSTACK; \
              bump_pos_cont state str ~max_pos ~pos PARSE) \
      | '\\' -> bump_pos_cont state str ~max_pos ~pos parse_escaped \
      | '\010' as c -> add_bump_line state str ~max_pos ~pos c parse_quoted \
      | '\013' as c -> add_bump_line state str ~max_pos ~pos c parse_quoted_nl \
      | c -> add_bump_pos state str ~max_pos ~pos c parse_quoted \
  \
  and parse_quoted_nl state str ~max_pos ~pos = \
    if pos > max_pos then mk_cont "parse_quoted_nl" parse_quoted_nl state \
    else \
      let pos = \
        let c = '\010' in \
        if GET_CHAR = c then ( \
          Buffer.add_char state.pbuf c; \
          pos + 1 \
        ) \
        else pos \
      in \
      parse_quoted state str ~max_pos ~pos \
  \
  and parse_escaped state str ~max_pos ~pos = \
    if pos > max_pos then mk_cont "parse_escaped" parse_escaped state \
    else \
      match GET_CHAR with \
      | '\010' -> bump_line_cont state str ~max_pos ~pos parse_skip_ws \
      | '\013' -> bump_line_cont state str ~max_pos ~pos parse_skip_ws_nl \
      | '0' .. '9' as c -> \
          bump_text_pos state; \
          let d = Char.code c - 48 in \
          parse_dec state str ~max_pos ~pos:(pos + 1) ~count:2 ~d \
      | 'x' -> \
          bump_text_pos state; \
          parse_hex state str ~max_pos ~pos:(pos + 1) ~count:2 ~d:0 \
      | ('\\' | '"' | '\'' ) as c -> \
          add_bump_pos state str ~max_pos ~pos c parse_quoted \
      | 'n' -> add_bump_pos state str ~max_pos ~pos '\n' parse_quoted \
      | 't' -> add_bump_pos state str ~max_pos ~pos '\t' parse_quoted \
      | 'b' -> add_bump_pos state str ~max_pos ~pos '\b' parse_quoted \
      | 'r' -> add_bump_pos state str ~max_pos ~pos '\r' parse_quoted \
      | c -> \
          Buffer.add_char state.pbuf '\\'; \
          add_bump_pos state str ~max_pos ~pos c parse_quoted \
  \
  and parse_skip_ws state str ~max_pos ~pos = \
    if pos > max_pos then mk_cont "parse_skip_ws" parse_skip_ws state \
    else \
      match GET_CHAR with \
      | ' ' | '\009' -> bump_pos_cont state str ~max_pos ~pos parse_skip_ws \
      | _ -> parse_quoted state str ~max_pos ~pos \
  \
  and parse_skip_ws_nl state str ~max_pos ~pos = \
    if pos > max_pos then mk_cont "parse_skip_ws_nl" parse_skip_ws_nl state \
    else \
      let pos = if GET_CHAR = '\010' then pos + 1 else pos in \
      parse_skip_ws state str ~max_pos ~pos \
  \
  and parse_dec state str ~max_pos ~pos ~count ~d = \
    if pos > max_pos then mk_cont "parse_dec" (parse_dec ~count ~d) state \
    else \
      match GET_CHAR with \
      | '0' .. '9' as c -> \
          let d = 10 * d + Char.code c - 48 in \
          if count = 1 then \
            if d > 255 then \
              let err_msg = sprintf "illegal decimal escape: \\%d" d in \
              raise_parse_error (MK_PARSE_STATE state) "parse_dec" pos err_msg \
            else \
              add_bump_pos state str ~max_pos ~pos (Char.chr d) parse_quoted \
          else ( \
            bump_text_pos state; \
            parse_dec state str ~max_pos ~pos:(pos + 1) ~count:(count - 1) ~d) \
      | c -> raise_unexpected_char (MK_PARSE_STATE state) "parse_dec" pos c \
  \
  and parse_hex state str ~max_pos ~pos ~count ~d = \
    if pos > max_pos then mk_cont "parse_hex" (parse_hex ~count ~d) state \
    else \
      match GET_CHAR with \
      | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' as c -> \
          let corr = \
            if c >= 'a' then 87 \
            else if c >= 'A' then 55 \
            else 48 \
          in \
          let d = 16 * d + Char.code c - corr in \
          if count = 1 then \
            if d > 255 then \
              let err_msg = sprintf "illegal hexadecimal escape: \\%x" d in \
              raise_parse_error (MK_PARSE_STATE state) "parse_hex" pos err_msg \
            else \
              add_bump_pos state str ~max_pos ~pos (Char.chr d) parse_quoted \
          else ( \
            bump_text_pos state; \
            parse_hex state str ~max_pos ~pos:(pos + 1) ~count:(count - 1) ~d) \
      | c -> raise_unexpected_char (MK_PARSE_STATE state) "parse_hex" pos c \
  \
  let PARSE ?(parse_pos = Parse_pos.create ()) ?len str = \
    let pos = parse_pos.Parse_pos.buf_pos in \
    let len = \
      match len with \
      | Some len -> len \
      | None -> GET_LEN str - pos \
    in \
    let max_pos = check_str_bounds "parse" ~pos ~len str in \
    let state = \
      { \
        parse_pos = parse_pos; \
        pstack = INIT_PSTACK; \
        pbuf = Buffer.create 128; \
      } \
    in \
    PARSE state str ~max_pos ~pos

MK_PARSER(
  string, String.length, parse_str, str.[pos],
  state.pstack, state.pstack <- pstack,
  ,,
  Atom pbuf_str, List sexp_lst, [], `Sexp
)

let parse = parse_str


(* Annot parsers *)

let get_glob_ofs parse_pos pos =
  parse_pos.Parse_pos.global_offset + pos - parse_pos.Parse_pos.buf_pos

let mk_annot_pos
      ({ Parse_pos.text_line = line; text_char = col } as parse_pos) pos =
  { Annot.line = line; col = col; offset = get_glob_ofs parse_pos pos }

let mk_annot_pos1
      ({ Parse_pos.text_line = line; text_char = col } as parse_pos) pos =
  { Annot.line = line; col = col + 1; offset = get_glob_ofs parse_pos pos }

let add_annot_pos { parse_pos = parse_pos; pstack = pstack } pos =
  pstack.Annot.positions <- mk_annot_pos parse_pos pos :: pstack.Annot.positions

let add_annot_pos1 { parse_pos = parse_pos; pstack = pstack } pos =
  pstack.Annot.positions <-
    mk_annot_pos1 parse_pos pos :: pstack.Annot.positions

let get_annot_range { parse_pos = parse_pos; pstack = pstack } pos =
  let start_pos =
    match pstack.Annot.positions with
    | [] -> assert false  (* impossible *)
    | h :: t -> pstack.Annot.positions <- t; h
  in
  let end_pos =
    {
      Annot.
      line = parse_pos.Parse_pos.text_line;
      col = parse_pos.Parse_pos.text_char;
      offset = get_glob_ofs parse_pos pos;
    }
  in
  { Annot.start_pos = start_pos; end_pos = end_pos }

let mk_annot_atom parse_state str pos =
  Annot.Atom (get_annot_range parse_state pos, Atom str)

let mk_annot_list parse_state annot_lst pos =
  let range = get_annot_range parse_state pos in
  let sexp = List (List.rev (List.rev_map Annot.get_sexp annot_lst)) in
  Annot.List (range, annot_lst, sexp)

let init_annot_pstate () = { Annot.positions = []; stack = [] }

MK_PARSER(
  string, String.length, parse_str_annot, str.[pos],
  state.pstack.Annot.stack, state.pstack.Annot.stack <- pstack,
  add_annot_pos state pos;,add_annot_pos1 state pos;,
  mk_annot_atom state pbuf_str pos, mk_annot_list state sexp_lst pos,
  init_annot_pstate (), `Annot
)


(* Partial parsing from bigstrings *)

(* NOTE: this is really an awful duplication of the code for parsing
   strings, but since OCaml does not inline higher-order functions known
   at compile, other solutions would sacrifice a lot of efficiency. *)

MK_PARSER(
  bigstring, Array1.dim, parse_bigstring, str.{pos},
  state.pstack, state.pstack <- pstack,
  ,,
  Atom pbuf_str, List sexp_lst, [], `Sexp
)

MK_PARSER(
  bigstring, Array1.dim, parse_bigstring_annot, str.{pos},
  state.pstack.Annot.stack, state.pstack.Annot.stack <- pstack,
  add_annot_pos state pos;,add_annot_pos1 state pos;,
  mk_annot_atom state pbuf_str pos, mk_annot_list state sexp_lst pos,
  init_annot_pstate (), `Annot
)


(* Input functions *)

let mk_this_parse ?parse_pos my_parse = (); fun ~pos ~len str ->
  let parse_pos =
    match parse_pos with
    | None -> Parse_pos.create ~buf_pos:pos ()
    | Some parse_pos -> parse_pos.Parse_pos.buf_pos <- pos; parse_pos
  in
  my_parse ?parse_pos:(Some parse_pos) ?len:(Some len) str

let gen_input_sexp my_parse ?parse_pos ic =
  let buf = String.create 1 in
  let rec loop this_parse =
    let c = input_char ic in
    buf.[0] <- c;
    match this_parse ~pos:0 ~len:1 buf with
    | Done (sexp, _) -> sexp
    | Cont (_, this_parse) -> loop this_parse
  in
  loop (mk_this_parse ?parse_pos my_parse)

let input_sexp ?parse_pos ic = gen_input_sexp parse ?parse_pos ic

let gen_input_rev_sexps my_parse ?parse_pos ?(buf = String.create 8192) ic =
  let rev_sexps_ref = ref [] in
  let buf_len = String.length buf in
  let rec loop this_parse ~pos ~len ~is_incomplete =
    if len > 0 then
      match this_parse ~pos ~len buf with
      | Done (sexp, ({ Parse_pos.buf_pos = buf_pos } as parse_pos)) ->
          rev_sexps_ref := sexp :: !rev_sexps_ref;
          let n_parsed = buf_pos - pos in
          let this_parse = mk_this_parse ~parse_pos my_parse in
          if n_parsed = len then
            let new_len = input ic buf 0 buf_len in
            loop this_parse ~pos:0 ~len:new_len ~is_incomplete:false
          else
            loop this_parse
              ~pos:buf_pos ~len:(len - n_parsed) ~is_incomplete:false
      | Cont (ws_only, this_parse) ->
          loop this_parse
            ~pos:0 ~len:(input ic buf 0 buf_len) ~is_incomplete:(not ws_only)
    else if is_incomplete then
      failwith
        "Sexplib.Sexp.input_rev_sexps: reached EOF with incomplete S-expression"
    else !rev_sexps_ref
  in
  let len = input ic buf 0 buf_len in
  let this_parse = mk_this_parse ?parse_pos my_parse in
  loop this_parse ~pos:0 ~len ~is_incomplete:false

let input_rev_sexps ?parse_pos ?buf ic =
  gen_input_rev_sexps parse ?parse_pos ?buf ic

let input_sexps ?parse_pos ?buf ic =
  List.rev (input_rev_sexps ?parse_pos ?buf ic)


(* of_string and of_bigstring *)

let of_string_bigstring loc this_parse ws_buf get_len get_sub str =
  match this_parse str with
  | Done (_, { Parse_pos.buf_pos = buf_pos }) when buf_pos <> get_len str ->
      let prefix_len = min (get_len str - buf_pos) 20 in
      let prefix = get_sub str buf_pos prefix_len in
      let msg =
        sprintf
          "Sexplib.Sexp.%s: S-expression followed by data at position %d: %S..."
          loc buf_pos prefix
      in
      failwith msg
  | Done (sexp, _) -> sexp
  | Cont (ws_only, this_parse) ->
      if ws_only then failwith (sprintf "Sexplib.Sexp.%s: whitespace only" loc);
      (* When parsing atoms, the incremental parser cannot tell whether
         it is at the end until it hits whitespace.  We therefore feed
         it one space to determine whether it is finished. *)
      match this_parse ~pos:0 ~len:1 ws_buf with
      | Done (sexp, _) -> sexp
      | Cont _ ->
          failwith (
            sprintf "Sexplib.Sexp.%s: got incomplete S-expression: %s"
              loc (get_sub str 0 (get_len str)))

let of_string str =
  of_string_bigstring "of_string" parse " " String.length String.sub str

let get_bstr_sub_str bstr pos len =
  let str = String.create len in
  for i = 0 to len - 1 do str.[i] <- bstr.{pos + i} done;
  str

let bstr_ws_buf = Array1.create char c_layout 1
let () = bstr_ws_buf.{0} <- ' '

let of_bigstring bstr =
  of_string_bigstring
    "of_bigstring" parse_bigstring bstr_ws_buf Array1.dim get_bstr_sub_str bstr


(* Loading *)

let gen_load_rev_sexps input_rev_sexps ?buf file =
  let ic = open_in file in
  try
    let sexps = input_rev_sexps ?parse_pos:None ?buf ic in
    close_in ic;
    sexps
  with exc -> close_in_noerr ic; raise exc

let load_rev_sexps ?buf file = gen_load_rev_sexps input_rev_sexps ?buf file

let load_sexps ?buf file = List.rev (load_rev_sexps ?buf file)

let gen_load_sexp my_parse ?(strict = true) ?(buf = String.create 8192) file =
  let buf_len = String.length buf in
  let ic = open_in file in
  let rec loop this_parse =
    let len = input ic buf 0 buf_len in
    if len = 0 then
      failwith (sprintf "Sexplib.Sexp.gen_load_sexp: end of file: %s" file)
    else
      match this_parse ~pos:0 ~len buf with
      | Done (sexp, ({ Parse_pos.buf_pos = buf_pos } as parse_pos))
        when strict ->
          let rec strict_loop this_parse ~pos ~len =
            match this_parse ~pos ~len buf with
            | Done _ | Cont (false, _) ->
                failwith (
                  sprintf
                    "Sexplib.Sexp.gen_load_sexp: more than one S-expression: %s"
                      file)
            | Cont (true, this_parse) ->
                let len = input ic buf 0 buf_len in
                if len = 0 then sexp
                else strict_loop this_parse ~pos:0 ~len
          in
          let this_parse = mk_this_parse ~parse_pos my_parse in
          strict_loop this_parse ~pos:buf_pos ~len:(len - buf_pos)
      | Done (sexp, _) -> sexp
      | Cont (_, this_parse) -> loop this_parse
  in
  try
    let sexp = loop (mk_this_parse my_parse) in
    close_in ic;
    sexp
  with exc -> close_in_noerr ic; raise exc

let load_sexp ?strict ?buf file = gen_load_sexp parse ?strict ?buf file

module Annotated = struct
  include Annot

  let parse = parse_str_annot
  let parse_bigstring = parse_bigstring_annot

  let input_rev_sexps ?parse_pos ?buf ic =
    gen_input_rev_sexps parse ?parse_pos ?buf ic

  let input_sexp ?parse_pos ic = gen_input_sexp parse ?parse_pos ic

  let input_sexps ?parse_pos ?buf ic =
    List.rev (input_rev_sexps ?parse_pos ?buf ic)

  let of_string str =
    of_string_bigstring
      "Annotated.of_string" parse " " String.length String.sub str

  let of_bigstring bstr =
    of_string_bigstring
      "Annotated.of_bigstring"
      parse_bigstring bstr_ws_buf Array1.dim get_bstr_sub_str bstr

  let load_rev_sexps ?buf file = gen_load_rev_sexps input_rev_sexps ?buf file
  let load_sexps ?buf file = List.rev (load_rev_sexps ?buf file)
  let load_sexp ?strict ?buf file = gen_load_sexp parse ?strict ?buf file

  let conv f annot_sexp =
    let sexp = get_sexp annot_sexp in
    try `Result (f sexp)
    with Of_sexp_error (exc, bad_sexp) as e ->
      match find_sexp annot_sexp bad_sexp with
      | None -> raise e
      | Some bad_annot_sexp -> `Error (exc, bad_annot_sexp)

  let get_conv_exn ~file ~exc annot_sexp =
    let range = get_range annot_sexp in
    let { start_pos = { line = line; col = col } } = range in
    let loc = sprintf "%s:%d:%d" file line col in
    Of_sexp_error (Annot.Conv_exn (loc, exc), get_sexp annot_sexp)
end

let load_sexp_conv ?(strict = true) ?(buf = String.create 8192) file f =
  let sexp = load_sexp ~strict ~buf file in
  try `Result (f sexp)
  with Of_sexp_error _ ->
    Annotated.conv f (Annotated.load_sexp ~strict ~buf file)

let raise_conv_exn ~file = function
  | `Result res -> res
  | `Error (exc, annot_sexp) ->
      raise (Annotated.get_conv_exn ~file ~exc annot_sexp)

let load_sexp_conv_exn ?strict ?buf file f =
  raise_conv_exn ~file (load_sexp_conv ?strict ?buf file f)

let load_sexps_conv ?(buf = String.create 8192) file f =
  let rev_sexps = load_rev_sexps ~buf file in
  try List.rev_map (fun sexp -> `Result (f sexp)) rev_sexps
  with Of_sexp_error _ as e ->
    match Annotated.load_rev_sexps ~buf file with
    | [] ->
        (* File is now empty - perhaps it was a temporary file handle? *)
        raise e
    | rev_annot_sexps ->
        List.rev_map (fun annot_sexp -> Annotated.conv f annot_sexp)
          rev_annot_sexps

let load_sexps_conv_exn ?(buf = String.create 8192) file f =
  let rev_sexps = load_rev_sexps ~buf file in
  try List.rev_map f rev_sexps
  with Of_sexp_error _ as e ->
    match Annotated.load_rev_sexps ~buf file with
    | [] ->
        (* File is now empty - perhaps it was a temporary file handle? *)
        raise e
    | rev_annot_sexps ->
        List.rev_map
          (fun annot_sexp -> raise_conv_exn ~file (Annotated.conv f annot_sexp))
          rev_annot_sexps

let gen_of_string_conv of_string annot_of_string str f =
  let sexp = of_string str in
  try `Result (f sexp)
  with Of_sexp_error _ -> Annotated.conv f (annot_of_string str)

let of_string_conv str f =
  gen_of_string_conv of_string Annotated.of_string str f

let of_bigstring_conv bstr f =
  gen_of_string_conv of_bigstring Annotated.of_bigstring bstr f

module Of_string_conv_exn = struct
  type t = { exc : exn; sexp : Type.t; sub_sexp : Type.t }

  exception E of t
end

let gen_of_string_conv_exn of_string str f =
  let sexp = of_string str in
  try f sexp
  with Of_sexp_error (exc, sub_sexp) ->
    raise (Of_string_conv_exn.E { Of_string_conv_exn.exc = exc; sexp = sexp; sub_sexp = sub_sexp })

let of_string_conv_exn str f = gen_of_string_conv_exn of_string str f
let of_bigstring_conv_exn bstr f = gen_of_string_conv_exn of_bigstring bstr f


(* Utilities for automated type conversions *)

let unit = List []

external sexp_of_t : t -> t = "%identity"
external t_of_sexp : t -> t = "%identity"


(* Utilities for conversion error handling *)

type found = [ `Found | `Pos of int * found ]
type search_result = [ `Not_found | found ]

let rec search_physical sexp ~contained =
  if sexp == contained then `Found
  else
    match sexp with
    | Atom _ -> `Not_found
    | List lst ->
        let rec loop i = function
          | [] -> `Not_found
          | h :: t ->
              let res = search_physical h ~contained in
              match res with
              | `Not_found -> loop (i + 1) t
              | #found as found -> `Pos (i, found)
        in
        loop 0 lst

let rec subst_found sexp ~subst = function
  | `Found -> subst
  | `Pos (pos, found) ->
      match sexp with
      | Atom _ ->
          failwith
            "Sexplib.Sexp.subst_search_result: atom when position requested"
      | List lst ->
          let rec loop acc pos = function
            | [] ->
                failwith
                  "Sexplib.Sexp.subst_search_result: \
                  short list when position requested"
            | h :: t when pos <> 0 -> loop (h :: acc) (pos - 1) t
            | h :: t ->
                List (List.rev_append acc (subst_found h ~subst found :: t))
          in
          loop [] pos lst
