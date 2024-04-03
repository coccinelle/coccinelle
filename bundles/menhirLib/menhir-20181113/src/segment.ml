# 25 "menhir-20181113/src/segment.mll"
 

  type tag =
    | Segment
    | Whitespace

  open Lexing


# 12 "menhir-20181113/src/segment.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\251\255\252\255\001\000\002\000\254\255\255\255\005\000\
    \253\255\006\000\008\000\252\255\253\255\010\000\254\255\255\255\
    ";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\004\000\001\000\255\255\255\255\002\000\
    \255\255\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    ";
  Lexing.lex_default =
   "\001\000\000\000\000\000\009\000\255\255\000\000\000\000\255\255\
    \000\000\009\000\011\000\000\000\000\000\255\255\000\000\000\000\
    ";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\006\000\005\000\008\000\005\000\004\000\007\000\008\000\
    \008\000\015\000\014\000\007\000\014\000\013\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \006\000\000\000\000\000\003\000\000\000\000\000\000\000\000\000\
    \015\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\015\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\255\255\000\000\000\000\000\000\000\000\255\255\000\000\
    \012\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\003\000\004\000\000\000\003\000\007\000\
    \009\000\010\000\010\000\009\000\013\000\010\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \010\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\010\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\003\000\255\255\255\255\255\255\255\255\009\000\255\255\
    \010\000\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec idle opening segments lexbuf =
   __ocaml_lex_idle_rec opening segments lexbuf 0
and __ocaml_lex_idle_rec opening segments lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 49 "menhir-20181113/src/segment.mll"
    ( idle opening segments lexbuf )
# 117 "menhir-20181113/src/segment.ml"

  | 1 ->
# 51 "menhir-20181113/src/segment.mll"
    ( new_line lexbuf; idle opening segments lexbuf )
# 122 "menhir-20181113/src/segment.ml"

  | 2 ->
# 53 "menhir-20181113/src/segment.mll"
    ( new_line lexbuf; idle opening segments lexbuf )
# 127 "menhir-20181113/src/segment.ml"

  | 3 ->
# 55 "menhir-20181113/src/segment.mll"
    ( let closing = lexbuf.lex_start_p in
      let segment = Whitespace, opening, closing in
      let segments = segment :: segments in
      List.rev segments )
# 135 "menhir-20181113/src/segment.ml"

  | 4 ->
# 60 "menhir-20181113/src/segment.mll"
    ( let closing = lexbuf.lex_start_p in
      let segment = Whitespace, opening, closing in
      let segments = segment :: segments in
      let opening = closing in
      busy segments opening false lexbuf )
# 144 "menhir-20181113/src/segment.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_idle_rec opening segments lexbuf __ocaml_lex_state

and busy segments opening just_saw_a_newline lexbuf =
   __ocaml_lex_busy_rec segments opening just_saw_a_newline lexbuf 10
and __ocaml_lex_busy_rec segments opening just_saw_a_newline lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 75 "menhir-20181113/src/segment.mll"
    ( busy segments opening just_saw_a_newline lexbuf )
# 156 "menhir-20181113/src/segment.ml"

  | 1 ->
# 77 "menhir-20181113/src/segment.mll"
    ( new_line lexbuf;
      (* The newline that we just saw is already included in the segment.
         This one is not included. *)
      let closing = lexbuf.lex_start_p in
      if just_saw_a_newline then
        let segment = Segment, opening, closing in
        let segments = segment :: segments in
        let opening = closing in
        idle opening segments lexbuf
      else
        busy segments opening true lexbuf )
# 171 "menhir-20181113/src/segment.ml"

  | 2 ->
# 89 "menhir-20181113/src/segment.mll"
    ( let closing = lexbuf.lex_start_p in
      let segment = Segment, opening, closing in
      let segments = segment :: segments in
      List.rev segments )
# 179 "menhir-20181113/src/segment.ml"

  | 3 ->
# 94 "menhir-20181113/src/segment.mll"
    ( busy segments opening false lexbuf )
# 184 "menhir-20181113/src/segment.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_busy_rec segments opening just_saw_a_newline lexbuf __ocaml_lex_state

;;

# 96 "menhir-20181113/src/segment.mll"
 

  (* This wrapper function reads a file, cuts it into segments, and
     creates a fresh lexbuf for each segment, taking care to adjust
     its start position. *)

  let segment filename : (tag * string * lexbuf) list =
    let content = IO.read_whole_file filename in
    let lexbuf = from_string content in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let segments : (tag * position * position) list =
      idle lexbuf.lex_curr_p [] lexbuf
    in
    List.map (fun (tag, startp, endp) ->
      let start = startp.pos_cnum in
      let length = endp.pos_cnum - start in
      let content = String.sub content start length in
      let lexbuf = from_string content in
      lexbuf.lex_start_p <- startp;
      lexbuf.lex_curr_p <- startp;
      lexbuf.lex_abs_pos <- startp.pos_cnum;
        (* That was tricky to find out. See [Lexing.engine]. [pos_cnum] is
           updated based on [buf.lex_abs_pos + buf.lex_curr_pos]. *)
      tag, content, lexbuf
    ) segments


# 219 "menhir-20181113/src/segment.ml"
