((a.res Ok) (a3d.res Ok) (a_and_e.res Ok) (a_and_e_ver1.res Ok) (ab.res Ok)
 (addaft.res Ok) (addbefore.res Ok) (addbeforeafter.res Ok) (addelse.res Ok)
 (addfield.res Ok) (addif.res Ok) (addif1.res Ok) (addif2.res Ok)
 (addtoo.res Ok) (after_if.res Ok) (anon.res Ok) (ar.res Ok) (arg.res Ok)
 (argument.res Ok) (arparam.res Ok) (array_init.res Ok) (array_size.res Ok)
 (arraysz.res Ok) (attradd.res Ok) (b1.res Ok) (b2.res Ok)
 (bad_iso_example.res
  (Pb
   "INCORRECT:diff token: ( VS x\nFile \"tests/bad_iso_example.c\", line 2, column 6,  charpos = 19\n    around = '(', whole content =   if ((x = 3)) return;\nFile \"tests/bad_iso_example.res\", line 2, column 6,  charpos = 19\n    around = 'x', whole content =   if (x) return;\n\n    diff (result(<) vs expected_result(>)) = \n    @@ -1,4 +1,4 @@\n     int main() {\n    -  if ((x = 3)) return;\n    +  if (x) return;\n     }\n     \n"))
 (bad_kfree.res Ok) (bad_ptr_print.res Ok) (bad_typedef.res Ok)
 (badexp.res Ok)
 (badpos.res
  (Pb
   "PROBLEM\n   exn = Failure(\"rule starting on line 1: already tagged token:\\nC code context\\nFile \\\"tests/badpos.c\\\", line 5, column 30,  charpos = 139\\n    around = 'reg_ptr', whole content = \\t (int) -(((struct pt_regs *) reg_ptr)->orig_eax + 2));\")\n"))
 (badtypedef.res Ok) (badwhen.res Ok) (badzero.res Ok) (befS.res Ok)
 (ben.res Ok) (bitfield.res Ok) (braces.res Ok) (break.res Ok) (bug1.res Ok)
 (bugloop.res Ok) (bugon.res Ok) (cards.res Ok) (cast.res Ok)
 (cast_iso.res Ok) (com.res Ok) (comadd.res Ok) (comments.res Ok)
 (compare.res Ok) (condexp.res Ok) (const.res Ok) (const1bis.res Ok)
 (const_adding.res Ok) (const_array.res Ok) (const_implicit_iso.res Ok)
 (constty.res Ok) (constx.res Ok) (cs_check.res Ok) (cst.res Ok)
 (cst_null.res Ok) (csw.res Ok)
 (dbg.res
  (Pb
   "INCORRECT:diff token: else VS (\nFile <COCCIOUTPUTFILE>, line 8, column 2,  charpos = 133\n    around = 'else', whole content = \t\telse pr = NULL;(\"PCI\");\nFile \"tests/dbg.res\", line 7, column 5,  charpos = 130\n    around = '(', whole content = \t\tDBG(\"PCI\");\n\n    diff (result(<) vs expected_result(>)) = \n    @@ -4,6 +4,6 @@\n     \tstruct resource *pr, *r = &dev->resource[idx];\n     \n     \tif (pr)\n    -\t\tDBG\n    -\t\telse pr = NULL;(\"PCI\");\n    +\t\tDBG(\"PCI\");\n    +\telse pr = NULL;\n     }\n"))
 (dbg1.res
  (Pb
   "INCORRECT:diff token: E VS (\nFile <COCCIOUTPUTFILE>, line 5, column 2,  charpos = 75\n    around = 'E', whole content = \t\tE = NULL;(\"PCI\");\nFile \"tests/dbg1.res\", line 4, column 5,  charpos = 72\n    around = '(', whole content = \t\tDBG(\"PCI\");\n\n    diff (result(<) vs expected_result(>)) = \n    @@ -1,6 +1,6 @@\n     \n     static inline void alloc_resource(struct pci_dev *dev, int idx)\n     {\n    -\t\tDBG\n    -\t\tE = NULL;(\"PCI\");\n    +\t\tDBG(\"PCI\");\n    +\t\tE = NULL;\n     }\n"))
 (dc_close.res Ok) (debug.res Ok) (dec.res Ok) (decl.res Ok) (decl2.res Ok)
 (decl_space.res Ok)
 (decl_split.res
  (Pb
   "INCORRECT:diff token: int VS }\nFile \"tests/decl_split.c\", line 2, column 8,  charpos = 27\n    around = 'int', whole content =         int x, y;\nFile \"tests/decl_split.res\", line 2, column 0,  charpos = 19\n    around = '}', whole content = }\n\n    diff (result(<) vs expected_result(>)) = \n    @@ -1,3 +1,2 @@\n     int func(int i) { \n    -        int x, y;\n     }\n"))
 (decl_star.res Ok)
 (declinv.res
  (Pb
   "PROBLEM\n   exn = Unix.Unix_error(20, \"stat\", \"tests/declinv.cocci\")\n"))
 (defe.res Ok) (define_exp.res Ok) (define_param.res Ok) (deftodo.res Ok)
 (delete_function.res Ok) (delp.res Ok) (deref.res Ok) (devlink.res Ok)
 (disjexpr.res Ok) (disjexpr_ver1.res Ok) (disjexpr_ver2.res Ok)
 (disjid.res Ok) (distribute.res Ok) (double.res Ok) (double_assign.res Ok)
 (double_lines.res Ok) (double_switch.res Ok) (doublepos.res Ok)
 (doubleswitch.res Ok) (doundo.res Ok) (dowhile.res Ok) (dropf.res Ok)
 (dropparam.res Ok) (eb1.res Ok) (edots.res Ok) (edots_ver1.res Ok)
 (empty.res Ok) (end_commas.res Ok) (endif.res Ok) (enum.res Ok)
 (exitc.res
  (Pb
   "PROBLEM\n   exn = Prepare_ocamlcocci.LinkFailure(\"/tmp/exitc141503.cmxs\")\n"))
 (exitp.res (Pb "PROBLEM\n   exn = Failure(\"no python\")\n")) (exp.res Ok)
 (expnest.res Ok) (expopt.res Ok) (expopt2.res Ok) (expopt3.res Ok)
 (expopt3_ver1.res Ok) (expopt3_ver2.res Ok) (fields.res Ok)
 (fieldsmin.res Ok) (find_long.res Ok) (fix_flow_need.res Ok)
 (fn_todo.res Ok) (fnptr.res Ok) (fnret.res Ok) (fnty.res Ok) (four.res Ok)
 (foura.res Ok) (fp.res Ok) (fsh.res Ok) (fun.res Ok)
 (gilles-question.res Ok) (gotobreak.res Ok) (hd.res Ok) (headers.res Ok)
 (hex.res Ok) (hil1.res Ok) (if.res Ok) (ifbr.res Ok) (ifd.res Ok)
 (ifdef1.res Ok) (ifdef2.res Ok) (ifdef3.res Ok) (ifdef4.res Ok)
 (ifdef5.res Ok) (ifdef6.res Ok) (ifdef6a.res Ok) (ifdefmeta.res Ok)
 (ifdefmeta1.res Ok) (ifdefmeta2.res Ok) (ifdefmeta3.res Ok)
 (ifdefmeta4.res
  (Pb
   "PROBLEM\n   exn = Unix.Unix_error(20, \"stat\", \"tests/ifdefmeta4.cocci\")\n"))
 (ifend.res Ok) (ifields.res Ok) (ifzer.res Ok) (inc.res Ok)
 (incdir.res
  (Pb
   "INCORRECT:diff token: x VS 12\nFile \"tests/incdir.c\", line 4, column 6,  charpos = 46\n    around = 'x', whole content =   foo(x);\nFile \"tests/incdir.res\", line 4, column 6,  charpos = 46\n    around = '12', whole content =   foo(12);\n\n    diff (result(<) vs expected_result(>)) = \n    @@ -1,5 +1,5 @@\n     #include \"sub/incdir2.c\"\n     \n     int main () {\n    -  foo(x);\n    +  foo(12);\n     }\n"))
 (incl.res Ok) (inclifdef.res Ok) (include.res Ok)
 (incompatible_value.res Ok)
 (incpos.res (Pb "PROBLEM\n   exn = Failure(\"no python\")\n"))
 (incpos1.res (Pb "PROBLEM\n   exn = Failure(\"no python\")\n"))
 (inherited.res Ok) (inherited_ver1.res Ok) (inhmet.res Ok) (inhpos.res Ok)
 (initializer.res Ok) (initializer_many_fields.res Ok) (inline.res Ok)
 (insdef.res Ok) (isococci.res Ok) (isotest.res Ok) (isotest2.res Ok)
 (iterator.res Ok)
 (jloop1.res
  (Pb
   "PROBLEM\n   exn = Failure(\"minus: parse error: \\n = File \\\"tests/jloop1.cocci\\\", line 10, column 3,  charpos = 129\\n    around = '...>', whole content =    ...>\\n\")\n"))
 (julia10.res Ok) (julia7.res Ok) (justremove.res Ok) (keep_comma.res Ok)
 (km.res Ok) (kmalloc.res Ok) (kmc.res Ok) (ktype.res Ok)
 (labels_metastatement.res Ok) (labels_metastatement_ver1.res Ok)
 (lid.res Ok) (line_before_last.res Ok) (list_test.res Ok) (local.res Ok)
 (localid.res Ok) (longconst.res Ok) (longlong.res Ok) (longlongint.res Ok)
 (loop.res Ok) (lvalue.res Ok) (macro.res Ok) (macro_int16.res Ok)
 (match_init.res Ok) (match_no_meta.res Ok) (max.res Ok) (memory.res Ok)
 (metahex.res
  (Pb
   "INCORRECT:diff token: f VS }\nFile \"tests/metahex.c\", line 2, column 2,  charpos = 15\n    around = 'f', whole content =   f(3);\nFile \"tests/metahex.res\", line 2, column 0,  charpos = 13\n    around = '}', whole content = }\n\n    diff (result(<) vs expected_result(>)) = \n    @@ -1,4 +1,2 @@\n     int main() {\n    -  f(3);\n    -  g(0x03);\n     }\n"))
 (metaruleelem.res Ok) (metastatement2.res Ok) (metastatement_for.res Ok)
 (metastatement_if.res Ok) (minenum.res Ok) (minfn.res Ok)
 (mini_null_ref.res Ok) (minstruct.res Ok) (minusdots.res Ok)
 (minusdots_ver1.res Ok)
 (multi_func1.res
  (Pb
   "PROBLEM\n   exn = Failure(\"minus: parse error: \\n = File \\\"tests/multi_func1.cocci\\\", line 12, column 2,  charpos = 102\\n    around = 'fn2', whole content =   fn2(...) {\\n\")\n"))
 (multichars.res Ok) (multiplus.res Ok) (multitype.res Ok)
 (multitypedef.res Ok) (multivars.res Ok) (na.res Ok) (nameless.res Ok)
 (nest.res Ok) (nestone.res Ok) (nestseq.res Ok) (neststruct.res Ok)
 (nl.res Ok) (nocast.res Ok) (not.res Ok) (notest.res Ok) (noty.res Ok)
 (null_bool.res
  (Pb
   "INCORRECT:diff token: ) VS !=\nFile <COCCIOUTPUTFILE>, line 2, column 8,  charpos = 22\n    around = ')', whole content =   if (12) return;\nFile \"tests/null_bool.res\", line 2, column 9,  charpos = 23\n    around = '!=', whole content =   if (12 != NULL) return;\n\n    diff (result(<) vs expected_result(>)) = \n    @@ -1,6 +1,6 @@\n     int main () {\n    -  if (12) return;\n    -  if (a && 12 && b) return;\n    +  if (12 != NULL) return;\n    +  if (a && 12 != NULL && b) return;\n       if (12) return;\n       if (a && 12 && b) return;\n       x = x + 20;\n"))
 (null_type.res Ok)
 (oddifdef.res
  (Pb
   "INCORRECT:diff token: #else\n VS x\nFile <COCCIOUTPUTFILE>, line 10, column 0,  charpos = 114\n    around = '#else\n', whole content = #else\nFile \"tests/oddifdef.res\", line 10, column 2,  charpos = 116\n    around = 'x', whole content =   x = 0;\n\n    diff (result(<) vs expected_result(>)) = \n    @@ -7,8 +7,9 @@\n         a = 5;\n     \n     #ifdef FOO\n    +  x = 0;\n     #else\n    -\n    +  x = 0;\n     #endif\n     }\n     \n    @@ -21,8 +22,9 @@\n         a = 3;\n     \n     #ifdef FOO\n    +  x = 0;\n     #else\n    -\n    +  x = 0;\n     #endif\n     }\n     \n    @@ -35,7 +37,8 @@\n     #endif\n     \n     #ifdef FOO\n    +  x = 0;\n     #else\n    -\n    +  x = 0;\n     #endif\n     }\n"))
 (of.res Ok) (oneline.res Ok) (opt.res Ok) (optional_qualifier.res Ok)
 (optional_storage.res Ok) (orexp.res Ok) (overshoot.res Ok) (param.res Ok)
 (param_end.res Ok) (param_ver1.res Ok) (parameters_dots.res Ok)
 (partial.res Ok)
 (pb_distribute_type.res
  (Pb
   "INCORRECT:PB parsing only in generated-file\n    diff (result(<) vs expected_result(>)) = \n    @@ -10,6 +10,6 @@\n     }\n     \n     int foo() {\n    -  int[45] *x;\n    +  int (*x)[45];\n       return 0;\n     }\n"))
 (pb_distribute_type2.res
  (Pb
   "INCORRECT:PB parsing only in generated-file\n    diff (result(<) vs expected_result(>)) = \n    @@ -1,5 +1,5 @@\n     int foo() {\n    -  int* x;\n    +  int *x;\n       return 0;\n     }\n     \n    @@ -10,6 +10,6 @@\n     }\n     \n     int foo() {\n    -  int x[45]*;\n    +  int (*x)[45];\n       return 0;\n     }\n"))
 (pb_distribute_type3.res
  (Pb "PROBLEM\n   exn = Failure(\"line 7: index 53 53 already used\\n\")\n"))
 (pb_distribute_type4.res Ok) (pb_params_iso.res Ok)
 (pb_parsing_macro.res Ok) (pb_tag_symbols.res Ok) (pmac.res Ok)
 (posiso.res Ok) (positionc.res Ok) (post.res Ok) (print_return.res Ok)
 (proto.res Ok) (proto2.res Ok) (proto_ver1.res Ok) (proto_ver2.res Ok)
 (protoassert.res Ok) (protox.res Ok) (ptrar.res Ok) (rcu3.res Ok)
 (regexp.res Ok) (regexp2.res Ok) (regexp3.res Ok) (remove_call.res Ok)
 (remstruct.res Ok) (replace_typedef.res Ok) (reserved.res Ok)
 (retmacro.res Ok) (rets.res Ok) (return.res Ok) (return_implicit.res Ok)
 (retval.res Ok) (retval2.res Ok) (same_expr.res Ok)
 (scope_problem.res
  (Pb
   "INCORRECT:diff token: } VS a\nFile <COCCIOUTPUTFILE>, line 4, column 2,  charpos = 42\n    around = '}', whole content =   }\nFile \"tests/scope_problem.res\", line 4, column 4,  charpos = 44\n    around = 'a', whole content =     a = 2;\n\n    diff (result(<) vs expected_result(>)) = \n    @@ -1,7 +1,6 @@\n     void main(int i) {\n       if(1) {\n         int a;\n    +    a = 2;\n       }\n    -\n    -\n     }\n"))
 (serio.res
  (Pb
   "INCORRECT:diff token: init_MUTEX VS mutex_init\nFile \"tests/serio.c\", line 7, column 1,  charpos = 130\n    around = 'init_MUTEX', whole content = \tinit_MUTEX(&serio->drv_sem);\nFile \"tests/serio.res\", line 7, column 1,  charpos = 130\n    around = 'mutex_init', whole content = \tmutex_init(&serio->new_lock);\n\n    diff (result(<) vs expected_result(>)) = \n    @@ -4,5 +4,5 @@\n     \n     static void serio_init_port(struct serio *serio)\n     {\n    -\tinit_MUTEX(&serio->drv_sem);\n    +\tmutex_init(&serio->new_lock);\n     }\n"))
 (shared_brace.res Ok) (signed.res Ok) (sis.res Ok) (sizeof.res Ok)
 (sizeof_julia.res Ok) (sizeptr.res Ok) (sizestar.res Ok) (skip.res Ok)
 (smallfn.res Ok) (sp.res Ok) (spaces.res Ok) (spacing.res Ok) (spl.res Ok)
 (starprint.res Ok) (static.res Ok) (stm1.res Ok) (stm10.res Ok)
 (stm10_ver1.res Ok) (stm2.res Ok) (stm3.res Ok) (stm4.res Ok) (stm5.res Ok)
 (stm6.res Ok) (stm7.res Ok) (stm8.res Ok) (stmt.res Ok) (str_init.res Ok)
 (strangeorder.res Ok) (strid.res Ok) (strid2.res Ok) (string.res Ok)
 (struct.res Ok) (struct_metavar.res Ok) (struct_typedef.res Ok)
 (structfoo.res Ok) (substruct.res Ok) (sw.res Ok) (switch.res Ok)
 (switchdecl.res Ok) (td.res Ok) (tern.res Ok) (test0.res Ok) (test1.res Ok)
 (test10.res Ok) (test10_ver1.res Ok) (test11.res Ok) (test11_ver1.res Ok)
 (test12.res Ok) (test2.res Ok) (test3.res Ok) (test4.res Ok) (test5.res Ok)
 (test5_ver1.res Ok) (test6.res Ok) (test7.res Ok) (test8.res Ok)
 (test9.res Ok) (test_s.res Ok) (test_unsigned_meta.res Ok)
 (three_types.res Ok) (threea.res Ok) (top.res Ok) (topdec.res Ok)
 (topdec_ver1.res Ok) (topdec_ver2.res Ok) (toplevel_macrostmt.res Ok)
 (toplevel_struct.res Ok) (tup.res Ok) (twoproto.res Ok) (ty.res Ok)
 (ty1.res Ok) (ty_tyexp.res Ok) (tydisj.res Ok) (tyex.res Ok) (type.res Ok)
 (type1.res Ok) (type_annotated.res Ok) (type_ver1.res Ok)
 (type_ver2.res
  (Pb
   "INCORRECT:PB parsing only in generated-file\n    diff (result(<) vs expected_result(>)) = \n    @@ -1,5 +1,5 @@\n     int foo() {\n    -  int[10] *x;\n    +  int *x[10];\n       return 0;\n     }\n     \n"))
 (typedef.res Ok)
 (typedef3.res
  (Pb
   "INCORRECT:diff token: link VS p_dev\nFile <COCCIOUTPUTFILE>, line 7, column 29,  charpos = 137\n    around = 'link', whole content = \tunsigned int iobase = info->link.io.BasePort1;\nFile \"tests/typedef3.res\", line 7, column 29,  charpos = 137\n    around = 'p_dev', whole content = \tunsigned int iobase = info->p_dev->io.BasePort1;\n\n    diff (result(<) vs expected_result(>)) = \n    @@ -4,7 +4,7 @@\n     \n     static void should_work(foo *info)\n     {\n    -\tunsigned int iobase = info->link.io.BasePort1;\n    +\tunsigned int iobase = info->p_dev->io.BasePort1;\n     }\n     \n     static void does_work(struct bluecard_info_t *info)\n"))
 (typedef_double.res Ok) (typeof.res Ok) (useless_cast.res Ok)
 (varargs.res Ok) (video1bis.res Ok) (void.res Ok) (vpos.res Ok)
 (whitespace.res Ok) (wierd_argument.res Ok)
 (wierdinit.res
  (Pb
   "seems incorrect, but only because of code that was not parsablediff token: dev_link_t VS struct\nFile \"tests/wierdinit.c\", line 4, column 1,  charpos = 27\n    around = 'dev_link_t', whole content = \tdev_link_t *link;\nFile \"tests/wierdinit.res\", line 4, column 1,  charpos = 27\n    around = 'struct', whole content = \tstruct pcmcia_device *link;\n"))
 (ws2.res Ok) (xfield.res Ok) (y2.res Ok) (zero.res Ok))