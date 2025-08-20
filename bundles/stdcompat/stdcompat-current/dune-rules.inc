(rule
  (targets stdcompat__hashtbl_ext.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__hashtbl_ext.mli.in)
    (action
      (with-stdin-from stdcompat__hashtbl_ext.mli.in
        (with-stdout-to stdcompat__hashtbl_ext.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__hashtbl_ext.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__hashtbl_ext.ml.in)
    (action
      (with-stdin-from stdcompat__hashtbl_ext.ml.in
        (with-stdout-to stdcompat__hashtbl_ext.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__init.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__init.mli.in)
    (action
      (with-stdin-from stdcompat__init.mli.in
        (with-stdout-to stdcompat__init.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__init.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__init.mli.in)
    (action
      (with-stdin-from stdcompat__init.mli.in
        (with-stdout-to stdcompat__init.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__root.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__root.mli.in)
    (action
      (with-stdin-from stdcompat__root.mli.in
        (with-stdout-to stdcompat__root.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__root.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__root.ml.in)
    (action
      (with-stdin-from stdcompat__root.ml.in
        (with-stdout-to stdcompat__root.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__tools.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__tools.mli.in)
    (action
      (with-stdin-from stdcompat__tools.mli.in
        (with-stdout-to stdcompat__tools.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__tools.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__tools.ml.in)
    (action
      (with-stdin-from stdcompat__tools.ml.in
        (with-stdout-to stdcompat__tools.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat_tests.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat_tests.ml.in)
    (action
      (with-stdin-from stdcompat_tests.ml.in
        (with-stdout-to stdcompat_tests.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stubs.c)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stubs.c.in)
    (action
      (with-stdin-from stdcompat__stubs.c.in
        (with-stdout-to stdcompat__stubs.c
          (run %{gen} --source-type c)))))
(rule
  (targets stdcompat__pervasives_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__pervasives_s.mli.in)
    (action
      (with-stdin-from stdcompat__pervasives_s.mli.in
        (with-stdout-to stdcompat__pervasives_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__arg_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__arg_s.mli.in)
    (action
      (with-stdin-from stdcompat__arg_s.mli.in
        (with-stdout-to stdcompat__arg_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__array_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__array_s.mli.in)
    (action
      (with-stdin-from stdcompat__array_s.mli.in
        (with-stdout-to stdcompat__array_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__arrayLabels_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__arrayLabels_s.mli.in)
    (action
      (with-stdin-from stdcompat__arrayLabels_s.mli.in
        (with-stdout-to stdcompat__arrayLabels_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__atomic_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__atomic_s.mli.in)
    (action
      (with-stdin-from stdcompat__atomic_s.mli.in
        (with-stdout-to stdcompat__atomic_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bool_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bool_s.mli.in)
    (action
      (with-stdin-from stdcompat__bool_s.mli.in
        (with-stdout-to stdcompat__bool_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__buffer_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__buffer_s.mli.in)
    (action
      (with-stdin-from stdcompat__buffer_s.mli.in
        (with-stdout-to stdcompat__buffer_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bytes_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bytes_s.mli.in)
    (action
      (with-stdin-from stdcompat__bytes_s.mli.in
        (with-stdout-to stdcompat__bytes_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bytesLabels_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bytesLabels_s.mli.in)
    (action
      (with-stdin-from stdcompat__bytesLabels_s.mli.in
        (with-stdout-to stdcompat__bytesLabels_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__char_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__char_s.mli.in)
    (action
      (with-stdin-from stdcompat__char_s.mli.in
        (with-stdout-to stdcompat__char_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__digest_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__digest_s.mli.in)
    (action
      (with-stdin-from stdcompat__digest_s.mli.in
        (with-stdout-to stdcompat__digest_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__domain_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__domain_s.mli.in)
    (action
      (with-stdin-from stdcompat__domain_s.mli.in
        (with-stdout-to stdcompat__domain_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__dynarray_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__dynarray_s.mli.in)
    (action
      (with-stdin-from stdcompat__dynarray_s.mli.in
        (with-stdout-to stdcompat__dynarray_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__either_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__either_s.mli.in)
    (action
      (with-stdin-from stdcompat__either_s.mli.in
        (with-stdout-to stdcompat__either_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__ephemeron_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__ephemeron_s.mli.in)
    (action
      (with-stdin-from stdcompat__ephemeron_s.mli.in
        (with-stdout-to stdcompat__ephemeron_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__filename_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__filename_s.mli.in)
    (action
      (with-stdin-from stdcompat__filename_s.mli.in
        (with-stdout-to stdcompat__filename_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__float_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__float_s.mli.in)
    (action
      (with-stdin-from stdcompat__float_s.mli.in
        (with-stdout-to stdcompat__float_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__format_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__format_s.mli.in)
    (action
      (with-stdin-from stdcompat__format_s.mli.in
        (with-stdout-to stdcompat__format_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__fun_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__fun_s.mli.in)
    (action
      (with-stdin-from stdcompat__fun_s.mli.in
        (with-stdout-to stdcompat__fun_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__hashtbl_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__hashtbl_s.mli.in)
    (action
      (with-stdin-from stdcompat__hashtbl_s.mli.in
        (with-stdout-to stdcompat__hashtbl_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__in_channel_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__in_channel_s.mli.in)
    (action
      (with-stdin-from stdcompat__in_channel_s.mli.in
        (with-stdout-to stdcompat__in_channel_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int_s.mli.in)
    (action
      (with-stdin-from stdcompat__int_s.mli.in
        (with-stdout-to stdcompat__int_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int32_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int32_s.mli.in)
    (action
      (with-stdin-from stdcompat__int32_s.mli.in
        (with-stdout-to stdcompat__int32_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int64_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int64_s.mli.in)
    (action
      (with-stdin-from stdcompat__int64_s.mli.in
        (with-stdout-to stdcompat__int64_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__lazy_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__lazy_s.mli.in)
    (action
      (with-stdin-from stdcompat__lazy_s.mli.in
        (with-stdout-to stdcompat__lazy_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__lexing_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__lexing_s.mli.in)
    (action
      (with-stdin-from stdcompat__lexing_s.mli.in
        (with-stdout-to stdcompat__lexing_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__list_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__list_s.mli.in)
    (action
      (with-stdin-from stdcompat__list_s.mli.in
        (with-stdout-to stdcompat__list_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__listLabels_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__listLabels_s.mli.in)
    (action
      (with-stdin-from stdcompat__listLabels_s.mli.in
        (with-stdout-to stdcompat__listLabels_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__map_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__map_s.mli.in)
    (action
      (with-stdin-from stdcompat__map_s.mli.in
        (with-stdout-to stdcompat__map_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__moreLabels_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__moreLabels_s.mli.in)
    (action
      (with-stdin-from stdcompat__moreLabels_s.mli.in
        (with-stdout-to stdcompat__moreLabels_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__nativeint_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__nativeint_s.mli.in)
    (action
      (with-stdin-from stdcompat__nativeint_s.mli.in
        (with-stdout-to stdcompat__nativeint_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__option_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__option_s.mli.in)
    (action
      (with-stdin-from stdcompat__option_s.mli.in
        (with-stdout-to stdcompat__option_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__out_channel_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__out_channel_s.mli.in)
    (action
      (with-stdin-from stdcompat__out_channel_s.mli.in
        (with-stdout-to stdcompat__out_channel_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__printexc_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__printexc_s.mli.in)
    (action
      (with-stdin-from stdcompat__printexc_s.mli.in
        (with-stdout-to stdcompat__printexc_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__printf_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__printf_s.mli.in)
    (action
      (with-stdin-from stdcompat__printf_s.mli.in
        (with-stdout-to stdcompat__printf_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__queue_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__queue_s.mli.in)
    (action
      (with-stdin-from stdcompat__queue_s.mli.in
        (with-stdout-to stdcompat__queue_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__random_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__random_s.mli.in)
    (action
      (with-stdin-from stdcompat__random_s.mli.in
        (with-stdout-to stdcompat__random_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__result_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__result_s.mli.in)
    (action
      (with-stdin-from stdcompat__result_s.mli.in
        (with-stdout-to stdcompat__result_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__seq_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__seq_s.mli.in)
    (action
      (with-stdin-from stdcompat__seq_s.mli.in
        (with-stdout-to stdcompat__seq_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__set_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__set_s.mli.in)
    (action
      (with-stdin-from stdcompat__set_s.mli.in
        (with-stdout-to stdcompat__set_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stack_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stack_s.mli.in)
    (action
      (with-stdin-from stdcompat__stack_s.mli.in
        (with-stdout-to stdcompat__stack_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__string_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__string_s.mli.in)
    (action
      (with-stdin-from stdcompat__string_s.mli.in
        (with-stdout-to stdcompat__string_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stringLabels_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stringLabels_s.mli.in)
    (action
      (with-stdin-from stdcompat__stringLabels_s.mli.in
        (with-stdout-to stdcompat__stringLabels_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__sys_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__sys_s.mli.in)
    (action
      (with-stdin-from stdcompat__sys_s.mli.in
        (with-stdout-to stdcompat__sys_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__uchar_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__uchar_s.mli.in)
    (action
      (with-stdin-from stdcompat__uchar_s.mli.in
        (with-stdout-to stdcompat__uchar_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__weak_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__weak_s.mli.in)
    (action
      (with-stdin-from stdcompat__weak_s.mli.in
        (with-stdout-to stdcompat__weak_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__unit_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__unit_s.mli.in)
    (action
      (with-stdin-from stdcompat__unit_s.mli.in
        (with-stdout-to stdcompat__unit_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stdlib_s.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stdlib_s.mli.in)
    (action
      (with-stdin-from stdcompat__stdlib_s.mli.in
        (with-stdout-to stdcompat__stdlib_s.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__pervasives_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__pervasives_s.mli.in)
    (action
      (with-stdin-from stdcompat__pervasives_s.mli.in
        (with-stdout-to stdcompat__pervasives_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__arg_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__arg_s.mli.in)
    (action
      (with-stdin-from stdcompat__arg_s.mli.in
        (with-stdout-to stdcompat__arg_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__array_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__array_s.mli.in)
    (action
      (with-stdin-from stdcompat__array_s.mli.in
        (with-stdout-to stdcompat__array_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__arrayLabels_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__arrayLabels_s.mli.in)
    (action
      (with-stdin-from stdcompat__arrayLabels_s.mli.in
        (with-stdout-to stdcompat__arrayLabels_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__atomic_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__atomic_s.mli.in)
    (action
      (with-stdin-from stdcompat__atomic_s.mli.in
        (with-stdout-to stdcompat__atomic_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bool_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bool_s.mli.in)
    (action
      (with-stdin-from stdcompat__bool_s.mli.in
        (with-stdout-to stdcompat__bool_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__buffer_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__buffer_s.mli.in)
    (action
      (with-stdin-from stdcompat__buffer_s.mli.in
        (with-stdout-to stdcompat__buffer_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bytes_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bytes_s.mli.in)
    (action
      (with-stdin-from stdcompat__bytes_s.mli.in
        (with-stdout-to stdcompat__bytes_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bytesLabels_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bytesLabels_s.mli.in)
    (action
      (with-stdin-from stdcompat__bytesLabels_s.mli.in
        (with-stdout-to stdcompat__bytesLabels_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__char_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__char_s.mli.in)
    (action
      (with-stdin-from stdcompat__char_s.mli.in
        (with-stdout-to stdcompat__char_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__digest_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__digest_s.mli.in)
    (action
      (with-stdin-from stdcompat__digest_s.mli.in
        (with-stdout-to stdcompat__digest_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__domain_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__domain_s.mli.in)
    (action
      (with-stdin-from stdcompat__domain_s.mli.in
        (with-stdout-to stdcompat__domain_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__dynarray_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__dynarray_s.mli.in)
    (action
      (with-stdin-from stdcompat__dynarray_s.mli.in
        (with-stdout-to stdcompat__dynarray_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__either_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__either_s.mli.in)
    (action
      (with-stdin-from stdcompat__either_s.mli.in
        (with-stdout-to stdcompat__either_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__ephemeron_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__ephemeron_s.mli.in)
    (action
      (with-stdin-from stdcompat__ephemeron_s.mli.in
        (with-stdout-to stdcompat__ephemeron_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__filename_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__filename_s.mli.in)
    (action
      (with-stdin-from stdcompat__filename_s.mli.in
        (with-stdout-to stdcompat__filename_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__float_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__float_s.mli.in)
    (action
      (with-stdin-from stdcompat__float_s.mli.in
        (with-stdout-to stdcompat__float_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__format_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__format_s.mli.in)
    (action
      (with-stdin-from stdcompat__format_s.mli.in
        (with-stdout-to stdcompat__format_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__fun_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__fun_s.mli.in)
    (action
      (with-stdin-from stdcompat__fun_s.mli.in
        (with-stdout-to stdcompat__fun_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__hashtbl_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__hashtbl_s.mli.in)
    (action
      (with-stdin-from stdcompat__hashtbl_s.mli.in
        (with-stdout-to stdcompat__hashtbl_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int_s.mli.in)
    (action
      (with-stdin-from stdcompat__int_s.mli.in
        (with-stdout-to stdcompat__int_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int32_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int32_s.mli.in)
    (action
      (with-stdin-from stdcompat__int32_s.mli.in
        (with-stdout-to stdcompat__int32_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int64_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int64_s.mli.in)
    (action
      (with-stdin-from stdcompat__int64_s.mli.in
        (with-stdout-to stdcompat__int64_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__lazy_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__lazy_s.mli.in)
    (action
      (with-stdin-from stdcompat__lazy_s.mli.in
        (with-stdout-to stdcompat__lazy_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__lexing_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__lexing_s.mli.in)
    (action
      (with-stdin-from stdcompat__lexing_s.mli.in
        (with-stdout-to stdcompat__lexing_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__list_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__list_s.mli.in)
    (action
      (with-stdin-from stdcompat__list_s.mli.in
        (with-stdout-to stdcompat__list_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__listLabels_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__listLabels_s.mli.in)
    (action
      (with-stdin-from stdcompat__listLabels_s.mli.in
        (with-stdout-to stdcompat__listLabels_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__map_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__map_s.mli.in)
    (action
      (with-stdin-from stdcompat__map_s.mli.in
        (with-stdout-to stdcompat__map_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__moreLabels_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__moreLabels_s.mli.in)
    (action
      (with-stdin-from stdcompat__moreLabels_s.mli.in
        (with-stdout-to stdcompat__moreLabels_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__nativeint_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__nativeint_s.mli.in)
    (action
      (with-stdin-from stdcompat__nativeint_s.mli.in
        (with-stdout-to stdcompat__nativeint_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__option_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__option_s.mli.in)
    (action
      (with-stdin-from stdcompat__option_s.mli.in
        (with-stdout-to stdcompat__option_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__printexc_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__printexc_s.mli.in)
    (action
      (with-stdin-from stdcompat__printexc_s.mli.in
        (with-stdout-to stdcompat__printexc_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__printf_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__printf_s.mli.in)
    (action
      (with-stdin-from stdcompat__printf_s.mli.in
        (with-stdout-to stdcompat__printf_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__queue_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__queue_s.mli.in)
    (action
      (with-stdin-from stdcompat__queue_s.mli.in
        (with-stdout-to stdcompat__queue_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__random_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__random_s.mli.in)
    (action
      (with-stdin-from stdcompat__random_s.mli.in
        (with-stdout-to stdcompat__random_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__result_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__result_s.mli.in)
    (action
      (with-stdin-from stdcompat__result_s.mli.in
        (with-stdout-to stdcompat__result_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__seq_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__seq_s.mli.in)
    (action
      (with-stdin-from stdcompat__seq_s.mli.in
        (with-stdout-to stdcompat__seq_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__set_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__set_s.mli.in)
    (action
      (with-stdin-from stdcompat__set_s.mli.in
        (with-stdout-to stdcompat__set_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stack_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stack_s.mli.in)
    (action
      (with-stdin-from stdcompat__stack_s.mli.in
        (with-stdout-to stdcompat__stack_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stdlib_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stdlib_s.mli.in)
    (action
      (with-stdin-from stdcompat__stdlib_s.mli.in
        (with-stdout-to stdcompat__stdlib_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__string_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__string_s.mli.in)
    (action
      (with-stdin-from stdcompat__string_s.mli.in
        (with-stdout-to stdcompat__string_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stringLabels_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stringLabels_s.mli.in)
    (action
      (with-stdin-from stdcompat__stringLabels_s.mli.in
        (with-stdout-to stdcompat__stringLabels_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__sys_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__sys_s.mli.in)
    (action
      (with-stdin-from stdcompat__sys_s.mli.in
        (with-stdout-to stdcompat__sys_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__uchar_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__uchar_s.mli.in)
    (action
      (with-stdin-from stdcompat__uchar_s.mli.in
        (with-stdout-to stdcompat__uchar_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__weak_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__weak_s.mli.in)
    (action
      (with-stdin-from stdcompat__weak_s.mli.in
        (with-stdout-to stdcompat__weak_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__in_channel_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__in_channel_s.mli.in)
    (action
      (with-stdin-from stdcompat__in_channel_s.mli.in
        (with-stdout-to stdcompat__in_channel_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__out_channel_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__out_channel_s.mli.in)
    (action
      (with-stdin-from stdcompat__out_channel_s.mli.in
        (with-stdout-to stdcompat__out_channel_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__unit_s.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__unit_s.mli.in)
    (action
      (with-stdin-from stdcompat__unit_s.mli.in
        (with-stdout-to stdcompat__unit_s.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__pervasives.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__pervasives.ml.in)
    (action
      (with-stdin-from stdcompat__pervasives.ml.in
        (with-stdout-to stdcompat__pervasives.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__arg.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__arg.ml.in)
    (action
      (with-stdin-from stdcompat__arg.ml.in
        (with-stdout-to stdcompat__arg.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__array.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__array.ml.in)
    (action
      (with-stdin-from stdcompat__array.ml.in
        (with-stdout-to stdcompat__array.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__arrayLabels.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__arrayLabels.ml.in)
    (action
      (with-stdin-from stdcompat__arrayLabels.ml.in
        (with-stdout-to stdcompat__arrayLabels.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__atomic.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__atomic.ml.in)
    (action
      (with-stdin-from stdcompat__atomic.ml.in
        (with-stdout-to stdcompat__atomic.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bool.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bool.ml.in)
    (action
      (with-stdin-from stdcompat__bool.ml.in
        (with-stdout-to stdcompat__bool.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__buffer.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__buffer.ml.in)
    (action
      (with-stdin-from stdcompat__buffer.ml.in
        (with-stdout-to stdcompat__buffer.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bytes.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bytes.ml.in)
    (action
      (with-stdin-from stdcompat__bytes.ml.in
        (with-stdout-to stdcompat__bytes.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bytesLabels.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bytesLabels.ml.in)
    (action
      (with-stdin-from stdcompat__bytesLabels.ml.in
        (with-stdout-to stdcompat__bytesLabels.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__char.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__char.ml.in)
    (action
      (with-stdin-from stdcompat__char.ml.in
        (with-stdout-to stdcompat__char.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__digest.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__digest.ml.in)
    (action
      (with-stdin-from stdcompat__digest.ml.in
        (with-stdout-to stdcompat__digest.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__domain.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__domain.ml.in)
    (action
      (with-stdin-from stdcompat__domain.ml.in
        (with-stdout-to stdcompat__domain.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__dynarray.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__dynarray.ml.in)
    (action
      (with-stdin-from stdcompat__dynarray.ml.in
        (with-stdout-to stdcompat__dynarray.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__either.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__either.ml.in)
    (action
      (with-stdin-from stdcompat__either.ml.in
        (with-stdout-to stdcompat__either.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__ephemeron.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__ephemeron.ml.in)
    (action
      (with-stdin-from stdcompat__ephemeron.ml.in
        (with-stdout-to stdcompat__ephemeron.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__filename.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__filename.ml.in)
    (action
      (with-stdin-from stdcompat__filename.ml.in
        (with-stdout-to stdcompat__filename.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__float.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__float.ml.in)
    (action
      (with-stdin-from stdcompat__float.ml.in
        (with-stdout-to stdcompat__float.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__format.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__format.ml.in)
    (action
      (with-stdin-from stdcompat__format.ml.in
        (with-stdout-to stdcompat__format.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__fun.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__fun.ml.in)
    (action
      (with-stdin-from stdcompat__fun.ml.in
        (with-stdout-to stdcompat__fun.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__hashtbl.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__hashtbl.ml.in)
    (action
      (with-stdin-from stdcompat__hashtbl.ml.in
        (with-stdout-to stdcompat__hashtbl.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int.ml.in)
    (action
      (with-stdin-from stdcompat__int.ml.in
        (with-stdout-to stdcompat__int.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int32.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int32.ml.in)
    (action
      (with-stdin-from stdcompat__int32.ml.in
        (with-stdout-to stdcompat__int32.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int64.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int64.ml.in)
    (action
      (with-stdin-from stdcompat__int64.ml.in
        (with-stdout-to stdcompat__int64.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__lazy.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__lazy.ml.in)
    (action
      (with-stdin-from stdcompat__lazy.ml.in
        (with-stdout-to stdcompat__lazy.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__lexing.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__lexing.ml.in)
    (action
      (with-stdin-from stdcompat__lexing.ml.in
        (with-stdout-to stdcompat__lexing.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__list.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__list.ml.in)
    (action
      (with-stdin-from stdcompat__list.ml.in
        (with-stdout-to stdcompat__list.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__listLabels.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__listLabels.ml.in)
    (action
      (with-stdin-from stdcompat__listLabels.ml.in
        (with-stdout-to stdcompat__listLabels.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__map.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__map.ml.in)
    (action
      (with-stdin-from stdcompat__map.ml.in
        (with-stdout-to stdcompat__map.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__moreLabels.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__moreLabels.ml.in)
    (action
      (with-stdin-from stdcompat__moreLabels.ml.in
        (with-stdout-to stdcompat__moreLabels.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__nativeint.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__nativeint.ml.in)
    (action
      (with-stdin-from stdcompat__nativeint.ml.in
        (with-stdout-to stdcompat__nativeint.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__option.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__option.ml.in)
    (action
      (with-stdin-from stdcompat__option.ml.in
        (with-stdout-to stdcompat__option.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__printexc.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__printexc.ml.in)
    (action
      (with-stdin-from stdcompat__printexc.ml.in
        (with-stdout-to stdcompat__printexc.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__printf.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__printf.ml.in)
    (action
      (with-stdin-from stdcompat__printf.ml.in
        (with-stdout-to stdcompat__printf.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__queue.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__queue.ml.in)
    (action
      (with-stdin-from stdcompat__queue.ml.in
        (with-stdout-to stdcompat__queue.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__random.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__random.ml.in)
    (action
      (with-stdin-from stdcompat__random.ml.in
        (with-stdout-to stdcompat__random.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__result.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__result.ml.in)
    (action
      (with-stdin-from stdcompat__result.ml.in
        (with-stdout-to stdcompat__result.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__seq.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__seq.ml.in)
    (action
      (with-stdin-from stdcompat__seq.ml.in
        (with-stdout-to stdcompat__seq.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__set.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__set.ml.in)
    (action
      (with-stdin-from stdcompat__set.ml.in
        (with-stdout-to stdcompat__set.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stack.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stack.ml.in)
    (action
      (with-stdin-from stdcompat__stack.ml.in
        (with-stdout-to stdcompat__stack.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stdlib.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stdlib.ml.in)
    (action
      (with-stdin-from stdcompat__stdlib.ml.in
        (with-stdout-to stdcompat__stdlib.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__string.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__string.ml.in)
    (action
      (with-stdin-from stdcompat__string.ml.in
        (with-stdout-to stdcompat__string.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stringLabels.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stringLabels.ml.in)
    (action
      (with-stdin-from stdcompat__stringLabels.ml.in
        (with-stdout-to stdcompat__stringLabels.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__sys.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__sys.ml.in)
    (action
      (with-stdin-from stdcompat__sys.ml.in
        (with-stdout-to stdcompat__sys.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__uchar.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__uchar.ml.in)
    (action
      (with-stdin-from stdcompat__uchar.ml.in
        (with-stdout-to stdcompat__uchar.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__weak.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__weak.ml.in)
    (action
      (with-stdin-from stdcompat__weak.ml.in
        (with-stdout-to stdcompat__weak.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__in_channel.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__in_channel.ml.in)
    (action
      (with-stdin-from stdcompat__in_channel.ml.in
        (with-stdout-to stdcompat__in_channel.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__out_channel.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__out_channel.ml.in)
    (action
      (with-stdin-from stdcompat__out_channel.ml.in
        (with-stdout-to stdcompat__out_channel.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__unit.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__unit.ml.in)
    (action
      (with-stdin-from stdcompat__unit.ml.in
        (with-stdout-to stdcompat__unit.ml
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__pervasives.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__pervasives.mli.in)
    (action
      (with-stdin-from stdcompat__pervasives.mli.in
        (with-stdout-to stdcompat__pervasives.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__arg.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__arg.mli.in)
    (action
      (with-stdin-from stdcompat__arg.mli.in
        (with-stdout-to stdcompat__arg.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__array.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__array.mli.in)
    (action
      (with-stdin-from stdcompat__array.mli.in
        (with-stdout-to stdcompat__array.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__arrayLabels.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__arrayLabels.mli.in)
    (action
      (with-stdin-from stdcompat__arrayLabels.mli.in
        (with-stdout-to stdcompat__arrayLabels.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__atomic.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__atomic.mli.in)
    (action
      (with-stdin-from stdcompat__atomic.mli.in
        (with-stdout-to stdcompat__atomic.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bool.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bool.mli.in)
    (action
      (with-stdin-from stdcompat__bool.mli.in
        (with-stdout-to stdcompat__bool.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__buffer.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__buffer.mli.in)
    (action
      (with-stdin-from stdcompat__buffer.mli.in
        (with-stdout-to stdcompat__buffer.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bytes.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bytes.mli.in)
    (action
      (with-stdin-from stdcompat__bytes.mli.in
        (with-stdout-to stdcompat__bytes.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__bytesLabels.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__bytesLabels.mli.in)
    (action
      (with-stdin-from stdcompat__bytesLabels.mli.in
        (with-stdout-to stdcompat__bytesLabels.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__char.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__char.mli.in)
    (action
      (with-stdin-from stdcompat__char.mli.in
        (with-stdout-to stdcompat__char.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__digest.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__digest.mli.in)
    (action
      (with-stdin-from stdcompat__digest.mli.in
        (with-stdout-to stdcompat__digest.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__domain.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__domain.mli.in)
    (action
      (with-stdin-from stdcompat__domain.mli.in
        (with-stdout-to stdcompat__domain.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__dynarray.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__dynarray.mli.in)
    (action
      (with-stdin-from stdcompat__dynarray.mli.in
        (with-stdout-to stdcompat__dynarray.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__either.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__either.mli.in)
    (action
      (with-stdin-from stdcompat__either.mli.in
        (with-stdout-to stdcompat__either.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__ephemeron.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__ephemeron.mli.in)
    (action
      (with-stdin-from stdcompat__ephemeron.mli.in
        (with-stdout-to stdcompat__ephemeron.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__filename.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__filename.mli.in)
    (action
      (with-stdin-from stdcompat__filename.mli.in
        (with-stdout-to stdcompat__filename.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__float.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__float.mli.in)
    (action
      (with-stdin-from stdcompat__float.mli.in
        (with-stdout-to stdcompat__float.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__format.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__format.mli.in)
    (action
      (with-stdin-from stdcompat__format.mli.in
        (with-stdout-to stdcompat__format.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__fun.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__fun.mli.in)
    (action
      (with-stdin-from stdcompat__fun.mli.in
        (with-stdout-to stdcompat__fun.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__hashtbl.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__hashtbl.mli.in)
    (action
      (with-stdin-from stdcompat__hashtbl.mli.in
        (with-stdout-to stdcompat__hashtbl.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int.mli.in)
    (action
      (with-stdin-from stdcompat__int.mli.in
        (with-stdout-to stdcompat__int.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int32.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int32.mli.in)
    (action
      (with-stdin-from stdcompat__int32.mli.in
        (with-stdout-to stdcompat__int32.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__int64.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__int64.mli.in)
    (action
      (with-stdin-from stdcompat__int64.mli.in
        (with-stdout-to stdcompat__int64.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__lazy.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__lazy.mli.in)
    (action
      (with-stdin-from stdcompat__lazy.mli.in
        (with-stdout-to stdcompat__lazy.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__lexing.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__lexing.mli.in)
    (action
      (with-stdin-from stdcompat__lexing.mli.in
        (with-stdout-to stdcompat__lexing.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__list.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__list.mli.in)
    (action
      (with-stdin-from stdcompat__list.mli.in
        (with-stdout-to stdcompat__list.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__listLabels.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__listLabels.mli.in)
    (action
      (with-stdin-from stdcompat__listLabels.mli.in
        (with-stdout-to stdcompat__listLabels.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__map.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__map.mli.in)
    (action
      (with-stdin-from stdcompat__map.mli.in
        (with-stdout-to stdcompat__map.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__moreLabels.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__moreLabels.mli.in)
    (action
      (with-stdin-from stdcompat__moreLabels.mli.in
        (with-stdout-to stdcompat__moreLabels.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__nativeint.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__nativeint.mli.in)
    (action
      (with-stdin-from stdcompat__nativeint.mli.in
        (with-stdout-to stdcompat__nativeint.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__option.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__option.mli.in)
    (action
      (with-stdin-from stdcompat__option.mli.in
        (with-stdout-to stdcompat__option.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__printexc.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__printexc.mli.in)
    (action
      (with-stdin-from stdcompat__printexc.mli.in
        (with-stdout-to stdcompat__printexc.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__printf.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__printf.mli.in)
    (action
      (with-stdin-from stdcompat__printf.mli.in
        (with-stdout-to stdcompat__printf.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__queue.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__queue.mli.in)
    (action
      (with-stdin-from stdcompat__queue.mli.in
        (with-stdout-to stdcompat__queue.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__random.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__random.mli.in)
    (action
      (with-stdin-from stdcompat__random.mli.in
        (with-stdout-to stdcompat__random.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__result.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__result.mli.in)
    (action
      (with-stdin-from stdcompat__result.mli.in
        (with-stdout-to stdcompat__result.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__seq.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__seq.mli.in)
    (action
      (with-stdin-from stdcompat__seq.mli.in
        (with-stdout-to stdcompat__seq.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__set.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__set.mli.in)
    (action
      (with-stdin-from stdcompat__set.mli.in
        (with-stdout-to stdcompat__set.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stack.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stack.mli.in)
    (action
      (with-stdin-from stdcompat__stack.mli.in
        (with-stdout-to stdcompat__stack.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stdlib.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stdlib.mli.in)
    (action
      (with-stdin-from stdcompat__stdlib.mli.in
        (with-stdout-to stdcompat__stdlib.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__string.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__string.mli.in)
    (action
      (with-stdin-from stdcompat__string.mli.in
        (with-stdout-to stdcompat__string.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__stringLabels.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__stringLabels.mli.in)
    (action
      (with-stdin-from stdcompat__stringLabels.mli.in
        (with-stdout-to stdcompat__stringLabels.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__sys.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__sys.mli.in)
    (action
      (with-stdin-from stdcompat__sys.mli.in
        (with-stdout-to stdcompat__sys.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__uchar.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__uchar.mli.in)
    (action
      (with-stdin-from stdcompat__uchar.mli.in
        (with-stdout-to stdcompat__uchar.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__weak.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__weak.mli.in)
    (action
      (with-stdin-from stdcompat__weak.mli.in
        (with-stdout-to stdcompat__weak.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__in_channel.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__in_channel.mli.in)
    (action
      (with-stdin-from stdcompat__in_channel.mli.in
        (with-stdout-to stdcompat__in_channel.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__out_channel.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__out_channel.mli.in)
    (action
      (with-stdin-from stdcompat__out_channel.mli.in
        (with-stdout-to stdcompat__out_channel.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat__unit.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat__unit.mli.in)
    (action
      (with-stdin-from stdcompat__unit.mli.in
        (with-stdout-to stdcompat__unit.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat.mli)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat.mli.in)
    (action
      (with-stdin-from stdcompat.mli.in
        (with-stdout-to stdcompat.mli
          (run %{gen} --source-type ocaml)))))
(rule
  (targets stdcompat.ml)
  (deps
    (:gen tools/stdcompatpp.exe) stdcompat.ml.in)
    (action
      (with-stdin-from stdcompat.ml.in
        (with-stdout-to stdcompat.ml
          (run %{gen} --source-type ocaml)))))
