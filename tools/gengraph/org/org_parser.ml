
# 1 "org_parser.mly"
  


# 7 "org_parser.ml"
exception Error

type token = 
  | TVIEW
  | TVERT
  | TTODO
  | TSTODO
  | TSTAR
  | TSLASH
  | TRAB
  | TPLUS
  | TORG
  | TLINB
  | TLAB
  | TInt of (
# 10 "org_parser.mly"
      (int)
# 25 "org_parser.ml"
)
  | TId of (
# 11 "org_parser.mly"
      (string)
# 30 "org_parser.ml"
)
  | TFACE
  | TEQUAL
  | TDASH
  | TCONFIG
  | TCOLON
  | TCOLE
  | TCOLB
  | EOL
  | EOF

and _menhir_jeton = token

let _eRR =
  Error

module MenhirInterpreter = MenhirLib.TableInterpreter.Make (struct
  
  exception Error = Error
  
  type token = _menhir_jeton
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | EOF ->
          22
      | EOL ->
          21
      | TCOLB ->
          20
      | TCOLE ->
          19
      | TCOLON ->
          18
      | TCONFIG ->
          17
      | TDASH ->
          16
      | TEQUAL ->
          15
      | TFACE ->
          14
      | TId _ ->
          13
      | TInt _ ->
          12
      | TLAB ->
          11
      | TLINB ->
          10
      | TORG ->
          9
      | TPLUS ->
          8
      | TRAB ->
          7
      | TSLASH ->
          6
      | TSTAR ->
          5
      | TSTODO ->
          4
      | TTODO ->
          3
      | TVERT ->
          2
      | TVIEW ->
          1
  
  let error_terminal =
    0
  
  let token2value : token -> Obj.t =
    fun _tok ->
      match _tok with
      | EOF ->
          Obj.repr ()
      | EOL ->
          Obj.repr ()
      | TCOLB ->
          Obj.repr ()
      | TCOLE ->
          Obj.repr ()
      | TCOLON ->
          Obj.repr ()
      | TCONFIG ->
          Obj.repr ()
      | TDASH ->
          Obj.repr ()
      | TEQUAL ->
          Obj.repr ()
      | TFACE ->
          Obj.repr ()
      | TId _v ->
          Obj.repr _v
      | TInt _v ->
          Obj.repr _v
      | TLAB ->
          Obj.repr ()
      | TLINB ->
          Obj.repr ()
      | TORG ->
          Obj.repr ()
      | TPLUS ->
          Obj.repr ()
      | TRAB ->
          Obj.repr ()
      | TSLASH ->
          Obj.repr ()
      | TSTAR ->
          Obj.repr ()
      | TSTODO ->
          Obj.repr ()
      | TTODO ->
          Obj.repr ()
      | TVERT ->
          Obj.repr ()
      | TVIEW ->
          Obj.repr ()
  
  let default_reduction =
    (8, "\000\000\021\022\000\000\000\000\000\000\020\000\011\000\000\000\000\000\015\000\000\018\000\000\017\000\000\016\000\t\000\000\000\025\000\000\003\000\000\000\026\019\000\002\001\000\000\000\000\023\000\000\000\000\000\000\000\005\024\000\013\000\014\000\007")
  
  let error =
    (23, "\004\000\130 \008\000\000\000\000\000\000\000\001\000\000\002\000\016\000\000\000\000\016\003\000 \000\008\000\000\000\000\024\001\000\000\000\000 \004\000\000\008\000\017\012\000\001\000\000\016\000\000\000\000\000\008\000\000@\000\000\000\000\000@\000\004\000\000\000\000\000\002\000\000 \000\000\000\000\016\002\000\000\000\000@\000\000\008\000\002\004 \000\000\000\004\000\000\008\000\000\000\000\000\000\004\000\000\008\000\004\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000\128\016@\016\000\000\000 \000\000\004\000\000\000\002\000\000@\000\000\000\002\002\000\000\008\000\000\002\002A\004\004\130\000\000\000\000\000\000 \004\016\000\000\000\000\000@\000\000\002\000A\000\000\000")
  
  let action =
    ((8, "\t\026\000\000\t\007,\012\003\024\000\003\000\028\020\021\028$\000 (\000&.\000*2\000\028\000>8\018\000BD\00002@\000\0000\000\000\007J<6\000R\\Bbf\005\005\000\000\007\000@\000\t\000"), (8, "B\006\186\013N&%\022\026Zf\226\021\202\013&\n%\021-\013\029\134\030\"*\014%>FJR:V^bjn~\130\142\146\154\158\162\174\190\194\198\206\210\214\218\222\251"))
  
  let lhs =
    (4, "\014\220\203\186\169\152\135ffT3\"\017")
  
  let goto =
    ((8, "\011 \000\000\020\000\000\000\012\000\000\006\000\011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\003\000\000\000\000\000\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\000\000\024\026\000\000\028\000\000\000\014\000"), (8, "#\029-\012&\031.\012*@\012\014\013\029<*<\030A\005>@=+;:"))
  
  let semantic_action =
    [|
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
          } = _menhir_stack in
        raise (MenhirLib.TableInterpreter.Accept _1));
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.startp = _startpos__4_;
          MenhirLib.EngineTypes.endp = _endpos__4_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = l;
            MenhirLib.EngineTypes.startp = _startpos_l_;
            MenhirLib.EngineTypes.endp = _endpos_l_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.semv = s;
              MenhirLib.EngineTypes.startp = _startpos_s_;
              MenhirLib.EngineTypes.endp = _endpos_s_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.startp = _startpos__1_;
                MenhirLib.EngineTypes.endp = _endpos__1_;
                MenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          } = _menhir_stack in
        let l : 'tv_link = Obj.magic l in
        let s : 'tv_status = Obj.magic s in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : 'tv_bug = 
# 28 "org_parser.mly"
                             ( (s, l) )
# 208 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.startp = _startpos__11_;
          MenhirLib.EngineTypes.endp = _endpos__11_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.startp = _startpos__10_;
            MenhirLib.EngineTypes.endp = _endpos__10_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.semv = t;
              MenhirLib.EngineTypes.startp = _startpos_t_;
              MenhirLib.EngineTypes.endp = _endpos_t_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.startp = _startpos__8_;
                MenhirLib.EngineTypes.endp = _endpos__8_;
                MenhirLib.EngineTypes.next = {
                  MenhirLib.EngineTypes.startp = _startpos__7_;
                  MenhirLib.EngineTypes.endp = _endpos__7_;
                  MenhirLib.EngineTypes.next = {
                    MenhirLib.EngineTypes.semv = ops;
                    MenhirLib.EngineTypes.startp = _startpos_ops_;
                    MenhirLib.EngineTypes.endp = _endpos_ops_;
                    MenhirLib.EngineTypes.next = {
                      MenhirLib.EngineTypes.semv = p;
                      MenhirLib.EngineTypes.startp = _startpos_p_;
                      MenhirLib.EngineTypes.endp = _endpos_p_;
                      MenhirLib.EngineTypes.next = {
                        MenhirLib.EngineTypes.startp = _startpos__4_;
                        MenhirLib.EngineTypes.endp = _endpos__4_;
                        MenhirLib.EngineTypes.next = {
                          MenhirLib.EngineTypes.startp = _startpos__3_;
                          MenhirLib.EngineTypes.endp = _endpos__3_;
                          MenhirLib.EngineTypes.next = {
                            MenhirLib.EngineTypes.startp = _startpos__2_;
                            MenhirLib.EngineTypes.endp = _endpos__2_;
                            MenhirLib.EngineTypes.next = {
                              MenhirLib.EngineTypes.state = _menhir_s;
                              MenhirLib.EngineTypes.startp = _startpos__1_;
                              MenhirLib.EngineTypes.endp = _endpos__1_;
                              MenhirLib.EngineTypes.next = _menhir_stack;
                              };
                            };
                          };
                        };
                      };
                    };
                  };
                };
              };
            };
          } = _menhir_stack in
        let t : 'tv_text = Obj.magic t in
        let ops : 'tv_list_org_parser_option_ = Obj.magic ops in
        let p : 'tv_path = Obj.magic p in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__11_ in
        let _v : 'tv_link = 
# 37 "org_parser.mly"
                                     ( (p, ops, t) )
# 276 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _startpos = _menhir_env.MenhirLib.EngineTypes.lexbuf.Lexing.lex_start_p in
        let _endpos = _startpos in
        let _v : 'tv_list_TId_ = 
# 114 "standard.mly"
    ( [] )
# 293 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
            };
          } = _menhir_stack in
        let xs : 'tv_list_TId_ = Obj.magic xs in
        let x : (
# 11 "org_parser.mly"
      (string)
# 320 "org_parser.ml"
        ) = Obj.magic x in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_TId_ = 
# 116 "standard.mly"
    ( x :: xs )
# 327 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _startpos = _menhir_env.MenhirLib.EngineTypes.lexbuf.Lexing.lex_start_p in
        let _endpos = _startpos in
        let _v : 'tv_list_bug_ = 
# 114 "standard.mly"
    ( [] )
# 344 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
            };
          } = _menhir_stack in
        let xs : 'tv_list_bug_ = Obj.magic xs in
        let x : 'tv_bug = Obj.magic x in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_bug_ = 
# 116 "standard.mly"
    ( x :: xs )
# 374 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _startpos = _menhir_env.MenhirLib.EngineTypes.lexbuf.Lexing.lex_start_p in
        let _endpos = _startpos in
        let _v : 'tv_list_org_parser_option_ = 
# 114 "standard.mly"
    ( [] )
# 391 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
            };
          } = _menhir_stack in
        let xs : 'tv_list_org_parser_option_ = Obj.magic xs in
        let x : 'tv_org_parser_option = Obj.magic x in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_org_parser_option_ = 
# 116 "standard.mly"
    ( x :: xs )
# 421 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _startpos = _menhir_env.MenhirLib.EngineTypes.lexbuf.Lexing.lex_start_p in
        let _endpos = _startpos in
        let _v : 'tv_list_path_elt_ = 
# 114 "standard.mly"
    ( [] )
# 438 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
            };
          } = _menhir_stack in
        let xs : 'tv_list_path_elt_ = Obj.magic xs in
        let x : 'tv_path_elt = Obj.magic x in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_path_elt_ = 
# 116 "standard.mly"
    ( x :: xs )
# 468 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _startpos = _menhir_env.MenhirLib.EngineTypes.lexbuf.Lexing.lex_start_p in
        let _endpos = _startpos in
        let _v : 'tv_list_tail_ = 
# 114 "standard.mly"
    ( [] )
# 485 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
            };
          } = _menhir_stack in
        let xs : 'tv_list_tail_ = Obj.magic xs in
        let x : 'tv_tail = Obj.magic x in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_list_tail_ = 
# 116 "standard.mly"
    ( x :: xs )
# 515 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.startp = _startpos__3_;
          MenhirLib.EngineTypes.endp = _endpos__3_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = bugs;
              MenhirLib.EngineTypes.startp = _startpos_bugs_;
              MenhirLib.EngineTypes.endp = _endpos_bugs_;
              MenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          } = _menhir_stack in
        let bugs : 'tv_list_bug_ = Obj.magic bugs in
        let _startpos = _startpos_bugs_ in
        let _endpos = _endpos__3_ in
        let _v : (
# 14 "org_parser.mly"
      (Ast_org.orgs)
# 547 "org_parser.ml"
        ) = 
# 21 "org_parser.mly"
                                                   ( bugs )
# 551 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = v;
          MenhirLib.EngineTypes.startp = _startpos_v_;
          MenhirLib.EngineTypes.endp = _endpos_v_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.startp = _startpos__4_;
            MenhirLib.EngineTypes.endp = _endpos__4_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.startp = _startpos__3_;
              MenhirLib.EngineTypes.endp = _endpos__3_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.startp = _startpos__2_;
                MenhirLib.EngineTypes.endp = _endpos__2_;
                MenhirLib.EngineTypes.next = {
                  MenhirLib.EngineTypes.state = _menhir_s;
                  MenhirLib.EngineTypes.startp = _startpos__1_;
                  MenhirLib.EngineTypes.endp = _endpos__1_;
                  MenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          } = _menhir_stack in
        let v : (
# 10 "org_parser.mly"
      (int)
# 588 "org_parser.ml"
        ) = Obj.magic v in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_org_parser_option = 
# 46 "org_parser.mly"
                                        ( Ast_org.Line v )
# 595 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = v;
          MenhirLib.EngineTypes.startp = _startpos_v_;
          MenhirLib.EngineTypes.endp = _endpos_v_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.startp = _startpos__4_;
            MenhirLib.EngineTypes.endp = _endpos__4_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.startp = _startpos__3_;
              MenhirLib.EngineTypes.endp = _endpos__3_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.startp = _startpos__2_;
                MenhirLib.EngineTypes.endp = _endpos__2_;
                MenhirLib.EngineTypes.next = {
                  MenhirLib.EngineTypes.state = _menhir_s;
                  MenhirLib.EngineTypes.startp = _startpos__1_;
                  MenhirLib.EngineTypes.endp = _endpos__1_;
                  MenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          } = _menhir_stack in
        let v : (
# 10 "org_parser.mly"
      (int)
# 632 "org_parser.ml"
        ) = Obj.magic v in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_org_parser_option = 
# 47 "org_parser.mly"
                                        ( Ast_org.ColB v )
# 639 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = v;
          MenhirLib.EngineTypes.startp = _startpos_v_;
          MenhirLib.EngineTypes.endp = _endpos_v_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.startp = _startpos__4_;
            MenhirLib.EngineTypes.endp = _endpos__4_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.startp = _startpos__3_;
              MenhirLib.EngineTypes.endp = _endpos__3_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.startp = _startpos__2_;
                MenhirLib.EngineTypes.endp = _endpos__2_;
                MenhirLib.EngineTypes.next = {
                  MenhirLib.EngineTypes.state = _menhir_s;
                  MenhirLib.EngineTypes.startp = _startpos__1_;
                  MenhirLib.EngineTypes.endp = _endpos__1_;
                  MenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          } = _menhir_stack in
        let v : (
# 10 "org_parser.mly"
      (int)
# 676 "org_parser.ml"
        ) = Obj.magic v in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_org_parser_option = 
# 48 "org_parser.mly"
                                        ( Ast_org.ColE v )
# 683 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.startp = _startpos__5_;
          MenhirLib.EngineTypes.endp = _endpos__5_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.startp = _startpos__4_;
            MenhirLib.EngineTypes.endp = _endpos__4_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.startp = _startpos__3_;
              MenhirLib.EngineTypes.endp = _endpos__3_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.startp = _startpos__2_;
                MenhirLib.EngineTypes.endp = _endpos__2_;
                MenhirLib.EngineTypes.next = {
                  MenhirLib.EngineTypes.state = _menhir_s;
                  MenhirLib.EngineTypes.startp = _startpos__1_;
                  MenhirLib.EngineTypes.endp = _endpos__1_;
                  MenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          } = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_org_parser_option = 
# 49 "org_parser.mly"
                                        ( Ast_org.Face   )
# 721 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = p;
          MenhirLib.EngineTypes.startp = _startpos_p_;
          MenhirLib.EngineTypes.endp = _endpos_p_;
          MenhirLib.EngineTypes.next = _menhir_stack;
          } = _menhir_stack in
        let p : 'tv_list_path_elt_ = Obj.magic p in
        let _startpos = _startpos_p_ in
        let _endpos = _endpos_p_ in
        let _v : 'tv_path = 
# 40 "org_parser.mly"
                                     ( "/" ^ (String.concat "/" p) )
# 745 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = e;
          MenhirLib.EngineTypes.startp = _startpos_e_;
          MenhirLib.EngineTypes.endp = _endpos_e_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.startp = _startpos__1_;
            MenhirLib.EngineTypes.endp = _endpos__1_;
            MenhirLib.EngineTypes.next = _menhir_stack;
            };
          } = _menhir_stack in
        let e : (
# 11 "org_parser.mly"
      (string)
# 770 "org_parser.ml"
        ) = Obj.magic e in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_path_elt = 
# 43 "org_parser.mly"
                                     ( e )
# 777 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
          } = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_status = 
# 32 "org_parser.mly"
                                     ( Ast_org.TODO )
# 799 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = s;
          MenhirLib.EngineTypes.startp = _startpos_s_;
          MenhirLib.EngineTypes.endp = _endpos_s_;
          MenhirLib.EngineTypes.next = _menhir_stack;
          } = _menhir_stack in
        let s : (
# 11 "org_parser.mly"
      (string)
# 820 "org_parser.ml"
        ) = Obj.magic s in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_status = 
# 33 "org_parser.mly"
                                     ( Ast_org.Unknow(s) )
# 827 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.startp = _startpos__4_;
          MenhirLib.EngineTypes.endp = _endpos__4_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.startp = _startpos__3_;
            MenhirLib.EngineTypes.endp = _endpos__3_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.startp = _startpos__2_;
              MenhirLib.EngineTypes.endp = _endpos__2_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.startp = _startpos__1_;
                MenhirLib.EngineTypes.endp = _endpos__1_;
                MenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          } = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : 'tv_tail = 
# 24 "org_parser.mly"
                                                   ()
# 861 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.startp = _startpos__7_;
          MenhirLib.EngineTypes.endp = _endpos__7_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.startp = _startpos__6_;
            MenhirLib.EngineTypes.endp = _endpos__6_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.startp = _startpos__5_;
              MenhirLib.EngineTypes.endp = _endpos__5_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.startp = _startpos__4_;
                MenhirLib.EngineTypes.endp = _endpos__4_;
                MenhirLib.EngineTypes.next = {
                  MenhirLib.EngineTypes.startp = _startpos__3_;
                  MenhirLib.EngineTypes.endp = _endpos__3_;
                  MenhirLib.EngineTypes.next = {
                    MenhirLib.EngineTypes.startp = _startpos__2_;
                    MenhirLib.EngineTypes.endp = _endpos__2_;
                    MenhirLib.EngineTypes.next = {
                      MenhirLib.EngineTypes.state = _menhir_s;
                      MenhirLib.EngineTypes.startp = _startpos__1_;
                      MenhirLib.EngineTypes.endp = _endpos__1_;
                      MenhirLib.EngineTypes.next = _menhir_stack;
                      };
                    };
                  };
                };
              };
            };
          } = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__7_ in
        let _v : 'tv_tail = 
# 25 "org_parser.mly"
                                                   ()
# 907 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = i;
          MenhirLib.EngineTypes.startp = _startpos_i_;
          MenhirLib.EngineTypes.endp = _endpos_i_;
          MenhirLib.EngineTypes.next = _menhir_stack;
          } = _menhir_stack in
        let i : (
# 11 "org_parser.mly"
      (string)
# 928 "org_parser.ml"
        ) = Obj.magic i in
        let _startpos = _startpos_i_ in
        let _endpos = _endpos_i_ in
        let _v : 'tv_text = 
# 52 "org_parser.mly"
                                        ( (i,0)    )
# 935 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = line;
          MenhirLib.EngineTypes.startp = _startpos_line_;
          MenhirLib.EngineTypes.endp = _endpos_line_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.startp = _startpos__3_;
            MenhirLib.EngineTypes.endp = _endpos__3_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.startp = _startpos__2_;
              MenhirLib.EngineTypes.endp = _endpos__2_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.semv = p;
                MenhirLib.EngineTypes.startp = _startpos_p_;
                MenhirLib.EngineTypes.endp = _endpos_p_;
                MenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          } = _menhir_stack in
        let line : (
# 10 "org_parser.mly"
      (int)
# 969 "org_parser.ml"
        ) = Obj.magic line in
        let p : 'tv_path = Obj.magic p in
        let _startpos = _startpos_p_ in
        let _endpos = _endpos_line_ in
        let _v : 'tv_text = 
# 53 "org_parser.mly"
                                        ( (p,line) )
# 977 "org_parser.ml"
         in
        _menhir_env.MenhirLib.EngineTypes.stack <- {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
          });
      |]
  
  let recovery =
    false
  
  let trace =
    None
  
  end)

let rec main =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 14 "org_parser.mly"
      (Ast_org.orgs)
# 1001 "org_parser.ml"
    ))



