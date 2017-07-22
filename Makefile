ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
	ifeq ($(wildcard Makefile.config),)
		ifeq ($(wildcard configure),)
			$(error \
				To make coccinelle,\
				please run ./autogen and ./configure first)
		else
			$(error \
				To make coccinelle,\
				please run ./configure first)
		endif
	else
		include Makefile.config
	endif
endif
endif

CORE_LIBRARIES:=\
	commons globals parsing_cocci parsing_c
LIBRARIES:=\
	$(CORE_LIBRARIES) ctl ocaml python engine popl09 extra

TOOLS:=spatch spgen

DEPEND_METHOD:=multifile

SOURCES_commons:=\
	ocamlextra/dumper.ml commands.ml common.ml ograph_simple.ml \
	ograph_extended.ml
SOURCES_globals:=\
	config.ml flag.ml iteration.ml $(REGEXP_FILE) regexp.ml
CLEAN_globals:=regexp_pcre regexp_str
SOURCES_ctl:=\
	flag_ctl.ml ast_ctl.ml pretty_print_ctl.ml ctl_engine.ml wrapper_ctl.ml
SOURCES_parsing_cocci:=\
	flag_parsing_cocci.ml ast_cocci.ml ast0_cocci.ml \
	pretty_print_cocci.ml visitor_ast0_types.ml \
	visitor_ast.ml visitor_ast0.ml ast0toast.ml unparse_ast0.ml \
	unify_ast.ml compute_lines.ml iso_pattern.ml comm_assoc.ml \
	iso_compile.ml single_statement.ml simple_assignments.ml \
	get_metas.ml stmtlist.ml check_meta.ml top_level.ml \
	type_infer.ml test_exps.ml unitary_ast0.ml arity.ml index.ml \
	context_neg.ml adjust_pragmas.ml insert_plus.ml function_prototypes.ml \
	semantic_cocci.ml data.ml free_vars.ml safe_for_multi_decls.ml \
	parse_printf.ml parse_aux.ml disjdistr.ml \
	parser_cocci_menhir.mly lexer_cocci.mll \
	lexer_cli.mll lexer_script.mll \
	cocci_grep.ml dpll.ml get_constants2.ml id_utils.ml git_grep.ml \
	adjacency.ml commas_on_lists.ml parse_cocci.ml command_line.ml
SOURCES_parsing_c:=\
	token_annot.ml flag_parsing_c.ml parsing_stat.ml \
	token_c.ml ast_c.ml includes.ml control_flow_c.ml \
	visitor_c.ml lib_parsing_c.ml control_flow_c_build.ml \
	pretty_print_c.ml semantic_c.ml lexer_parser.ml parser_c.mly lexer_c.mll \
	parse_string_c.ml token_helpers.ml token_views_c.ml \
	cpp_token_c.ml parsing_hacks.ml cpp_analysis_c.ml \
	unparse_cocci.ml parsing_recovery_c.ml parsing_consistency_c.ml \
	danger.ml parse_c.ml unparse_c.ml unparse_hrule.ml \
	type_c.ml cpp_ast_c.ml \
	type_annoter_c.ml comment_annotater_c.ml \
	compare_c.ml test_parsing_c.ml
SOURCES_ocaml:=\
	externalanalysis.ml \
	exposed_modules.ml coccilib.ml ocamlcocci_aux.ml $(OCAMLCOCCI_FILE) \
	prepare_ocamlcocci.ml run_ocamlcocci.ml
CLEAN_ocaml:=yes_prepare_ocamlcocci.ml no_prepare_ocamlcocci.ml
SOURCES_python:=\
	pycocci_aux.ml $(PYCOCCI_FILE) pycocci.ml
CLEAN_python:=yes_pycocci.ml no_pycocci.ml
SOURCES_engine:=\
	flag_matcher.ml lib_engine.ml pretty_print_engine.ml \
	check_exhaustive_pattern.ml \
	check_reachability.ml \
	c_vs_c.ml isomorphisms_c_c.ml \
	cocci_vs_c.ml pattern_c.ml transformation_c.ml \
	asttomember.ml asttoctl2.ml ctltotex.ml \
	postprocess_transinfo.ml ctlcocci_integration.ml
SOURCES_popl09:=\
	ast_popl.ml asttopopl.ml insert_quantifiers.ml \
	pretty_print_popl.ml flag_popl.ml popltoctl.ml popl.ml
SOURCES_extra:=\
	classic_patch.ml kbuild.ml maintainers.ml

SOURCES_spatch:=\
	flag_cocci.ml cocci.ml testing.ml read_options.ml main.ml

SOURCES_spgen:=\
	globals.ml ast_tostring.ml detect_patch.ml meta_variable.ml \
	snapshot.ml user_input.ml position_generator.ml disj_generator.ml \
	rule_body.ml rule_header.ml context_rule.ml script_rule.ml \
	file_transform.ml spgen_interactive.ml spgen_lexer.mll spgen_config.ml \
	spgen.ml spgen_test.ml

PREFIX_spatch:=

PREFIX_spgen:=tools/spgen/source/

LIBRARIES_spatch:=$(LIBRARIES)

LIBRARIES_spgen:=$(CORE_LIBRARIES)

CORE_BUNDLES=menhirLib pcre

ALL_BUNDLES=$(CORE_BUNDLES) parmap pyml

BUNDLES_spatch=$(ALL_BUNDLES)

BUNDLES_spgen=$(CORE_BUNDLES)

SOURCEFILES:=

define foreach_library
SOURCEFILES_$(library):=$(addprefix $(library)/, $(SOURCES_$(library)))
CLEANFILES_$(library):=$(addprefix $(library)/, $(CLEAN_$(library)))
SOURCEFILES+=$$(SOURCEFILES_$(library))
endef
$(foreach library,$(LIBRARIES),$(eval $(foreach_library)))

define foreach_tool
SOURCEFILES_$(tools):=$(addprefix $(PREFIX_$(tool)), $(SOURCES_$(tools)))
SOURCEFILES+=$$(SOURCEFILES_$(tool))
endef
$(foreach tool,$(TOOLS),$(eval $(foreach_tool)))

ifeq ($(TARGET_SPATCH),opt-only)
NATIVE:=yes
else
NATIVE:=no
endif

ifeq ($(NATIVE),yes)
	TOOLS_SUFFIX:=.opt
else
	TOOLS_SUFFIX:=
endif

EXPOSED_MODULES:=$(basename \
	$(shell sed -n 's/^.*(\* \(.*\) \*).*$$/\1/p' ocaml/exposed_modules.ml))

COMPILED_EXPOSED_MODULES:=\
	$(foreach EXT,cmi $(ALL_OBJECTS),\
		$(patsubst %,ocaml/%.$(EXT),$(notdir $(EXPOSED_MODULES))))

SEARCH_PATHS:=\
	commons/ocamlextra $(LIBRARIES) $(PREFIX_spgen) $(PCREDIR) $(PYMLDIR) \
	$(PARMAPDIR)

SEARCH_PATH_FLAGS:=$(addprefix -I ,$(SEARCH_PATHS))

OCAMLC_CMD:=$(OCAMLC) $(SEARCH_PATH_FLAGS)

OCAMLOPT_CMD:=$(OCAMLOPT) $(SEARCH_PATH_FLAGS)

OCAMLDEP_CMD:=$(OCAMLDEP) $(SEARCH_PATH_FLAGS) \
	$(addprefix -ml-synonym ,.mll .mly) \
	$(addprefix -mli-synonym ,.ml .mll .mly)

MENHIR_DEP_CMD:=$(MENHIR) --ocamldep "$(OCAMLDEP_CMD)" --depend

MENHIR_CMD:=$(MENHIR) --ocamlc "$(OCAMLC_CMD)" --explain --infer

.PHONY: .all
all: \
	$(foreach tool,$(TOOLS),$(PREFIX_$(tool))$(tool)$(TOOLS_SUFFIX)) \
	$(COMPILED_EXPOSED_MODULES)

.PHONY: clean
clean:
	rm -f .depend

ml_files_but_parsers:=\
	$(filter %.ml,$(SOURCEFILES)) \
	$(patsubst %.mll,%.ml,$(filter %.mll,$(SOURCEFILES)))

.PHONY: mlis
mlis: $(patsubst %.ml,%.mli,$(ml_files_but_parsers))

define foreach_ml_files_but_parsers
ifeq ($(wildcard $(sourcefile:.ml=.mli)),)
$(sourcefile:.ml=.mli): $(sourcefile)
	$(OCAMLC_CMD) -i $$< >$$@ || (rm $$@; false)
endif
endef
$(foreach sourcefile,$(ml_files_but_parsers),\
	$(eval $(foreach_ml_files_but_parsers)))

%.ml.d: %.ml
	$(OCAMLDEP_CMD) $< >$@ || (rm $@; false)

%.mli.d: %.mli
	$(OCAMLDEP_CMD) $< >$@ || (rm $@; false)

%.ml: %.mll
	$(OCAMLLEX) $<

ml_files:=$(ml_files_but_parsers) parsing_c/parser_c.ml
ml_and_mli_files:=$(ml_files) $(ml_files:.ml=.mli)

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
ifeq ($(DEPEND_METHOD),onefile)
.depend: $(ml_and_mli_files) parsing_cocci/parser_cocci_menhir.mly
	$(OCAMLDEP_CMD) $(ml_and_mli_files) >$@ || (rm $@; false)
	$(MENHIR_DEP_CMD) parsing_cocci/parser_cocci_menhir.mly >>$@ || (rm $@; false)

-include .depend
else ifeq ($(DEPEND_METHOD),multifile)
-include $(addsuffix .d,$(ml_and_mli_files))
-include parsing_cocci/parser_cocci_menhir.mly.d
else
$(error DEPEND_METHOD is expected to be 'onefile' or 'multifile',\
	but got $(DEPEND_METHOD))
endif
endif
endif

ifeq ($(DEPEND_METHOD),onefile)
%.cmi: %.mli .depend
	$(OCAMLC_CMD) -c $<

%.cmo: %.ml .depend
	$(OCAMLC_CMD) -c $<

%.cmx: %.ml .depend
	$(OCAMLOPT_CMD) -c $<
else
%.cmi: %.mli %.mli.d
	$(OCAMLC_CMD) -c $<

%.cmo: %.ml %.ml.d
	$(OCAMLC_CMD) -c $<

%.cmx: %.ml %.ml.d
	$(OCAMLOPT_CMD) -c $<
endif

## Parser_c

parsing_c/parser_c.ml: parsing_c/parser_c.mly
	$(OCAMLYACC) $<
parsing_c/parser_c.mli: parsing_c/parser_c.ml

## Parser_cocci_menhir

parsing_cocci/parser_cocci_menhir.mly.d: parsing_cocci/parser_cocci_menhir.mly
	$(MENHIR_DEP_CMD) $< >$@ || (rm $@; false)

ifeq ($(DEPEND_METHOD),onefile)
parsing_cocci/parser_cocci_menhir.ml: \
		parsing_cocci/parser_cocci_menhir.mly \
		bundles-menhirLib$(TOOLS_SUFFIX) \
		.depend
	$(MENHIR_CMD) $<
else
parsing_cocci/parser_cocci_menhir.ml: \
		parsing_cocci/parser_cocci_menhir.mly \
		bundles-menhirLib$(TOOLS_SUFFIX) \
		parsing_cocci/parser_cocci_menhir.mly.d
	$(MENHIR_CMD) $<
endif
parsing_cocci/parser_cocci_menhir.mli: parsing_cocci/parser_cocci_menhir.ml

## Bundles

# For each $(bundle), targets bundles-$(bundle), bundles-$(bundle).opt,
# and clean-$(bundle) are defined. The targets bundles, bundles.opt and
# clean-bundles run all of them.

.PHONY: bundles
bundles: $(ALL_BUNDLES:%=bundles-%)

.PHONY: bundles.opt
bundles.opt: $(ALL_BUNDLES:%=bundles-%.opt)

.PHONY: clean-bundles
clean-bundles: $(ALL_BUNDLES:%=clean-%)

.PHONY: distclean-bundles
distclean-bundles: $(ALL_BUNDLES:%=clean-%)

define foreach_bundle
.PHONY: bundles-$(bundle)
bundles-$(bundle):
	if echo $(MAKELIBS) | grep -q "/$(bundle)[ /]"; then \
		$(MAKE) -C bundles/$(bundle) all; \
	fi

.PHONY: bundles-$(bundle).opt
bundles-$(bundle).opt:
	if echo $(MAKELIBS) | grep -q "/$(bundle)[ /]"; then \
		$(MAKE) -C bundles/$(bundle) all.opt; \
	fi

.PHONY: clean-$(bundle)
clean-$(bundle):
	$(MAKE) -C bundles/$(bundle) clean

.PHONY: distclean-$(bundle)
distclean-$(bundle):
	$(MAKE) -C bundles/$(bundle) distclean

clean: clean-$(bundle)
endef
$(foreach bundle,$(ALL_BUNDLES),$(eval $(foreach_bundle)))

main.cmo: bundles-parmap
main.cmx: bundles-parmap.opt

python/yes_pycocci.cmi: bundles-pyml$(TOOLS_SUFFIX)
python/yes_pycocci.cmo: bundles-pyml
python/yes_pycocci.cmx: bundles-pyml.opt

globals/regexp_pcre.cmi: bundles-pcre$(TOOLS_SUFFIX)
globals/regexp_pcre.cmo: bundles-pcre
globals/regexp_pcre.cmx: bundles-pcre.opt

## Libraries

clean_sourcefile=$(foreach ext,cmi cmo cmx cmt cmti o ml.d mli.d,\
	$(basename $(sourcefile)).$(ext))

define foreach_library
clean: clean-$(library)

.PHONY: clean-$(library)
clean-$(library):
	rm -f $(library)/$(library).cma $(library)/$(library).cmxa \
		$(foreach sourcefile,$(SOURCEFILES_$(library)),$(clean_sourcefile)) \
		$(patsubst %.mll,%.ml,$(filter %.mll,$(SOURCEFILES_$(library)))) \
		$(patsubst %.mly,%.ml,$(filter %.mly,$(SOURCEFILES_$(library)))) \
		$(patsubst %.mly,%.mli,$(filter %.mly,$(SOURCEFILES_$(library)))) \
		$(foreach sourcefile,$(CLEANFILES_$(library)),$(clean_sourcefile))

$(library)/$(library).cmxa: $(addsuffix .cmx,$(basename $(SOURCEFILES_$(library))))
	$(OCAMLOPT_CMD) -a $$^ -o $$@

$(library)/$(library).cma: $(addsuffix .cmo,$(basename $(SOURCEFILES_$(library))))
	$(OCAMLC_CMD) -a $$^ -o $$@
endef
$(foreach library,$(LIBRARIES),$(eval $(foreach_library)))

define foreach_tool
clean: clean-$(tool)

.PHONY: clean-$(tool)
clean-$(tool):
	rm -f $(PREFIX_$(tool))$(tool) $(PREFIX_$(tool))$(tool).opt \
		$(foreach sourcefile,$(SOURCEFILES_$(tool)),$(clean_sourcefile))

$(PREFIX_$(tool))$(tool): \
		$(foreach library,$(LIBRARIES_$(tool)),$(library)/$(library).cma) \
		$(addsuffix .cmo,$(basename $(SOURCEFILES_$(tool))))
	$(OCAMLC_CMD) $(LIBS_$(tool):=.cma) $$^ -o $$@

$(PREFIX_$(tool))$(tool).opt: \
		$(foreach library,$(LIBRARIES_$(tool)),$(library)/$(library).cmxa) \
		$(addsuffix .cmx,$(basename $(SOURCEFILES_$(tool))))
	$(OCAMLOPT_CMD) $(LIBS_$(tool):=.cmxa) $$^ -o $$@
endef
$(foreach tool,$(TOOLS),$(eval $(foreach_tool)))

define copy_exposed_module
ocaml/$(notdir $(basename $(module))).$(EXT): $(basename $(module)).$(EXT)
	cp $$< $$@
endef

$(foreach module,$(EXPOSED_MODULES),\
	$(foreach EXT,cmi $(ALL_OBJECTS),$(eval $(copy_exposed_module))))
