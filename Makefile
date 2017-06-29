ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
	ifeq ($(wildcard Makefile.config),)
		ifeq ($(wildcard configure),)
			ERROR:=$(error \
				To make coccinelle,\
				please run ./autogen and ./configure first)
		else
			ERROR:=$(error \
				To make coccinelle,\
				please run ./configure first)
		endif
	else
		include Makefile.config
	endif
endif
endif

ifeq ($(TARGET_SPATCH),opt-only)
NATIVE=yes
else
NATIVE=no
endif

CORE_BUNDLES=menhirLib pcre

ALL_BUNDLES=$(CORE_BUNDLES) parmap pyml

BUNDLES_spatch=$(ALL_BUNDLES)

BUNDLES_spgen=$(CORE_BUNDLES)

OCAMLOPT_WITH_FLAGS=$(OCAMLOPT) $(EXTRA_OCAML_FLAGS) $(SEARCH_PATH_FLAGS)
OCAMLC_WITH_FLAGS=$(OCAMLC) $(EXTRA_OCAML_FLAGS) $(SEARCH_PATH_FLAGS)
OCAMLDEP_WITH_FLAGS=\
	$(OCAMLDEP) $(SEARCH_PATH_FLAGS) $(MODULES:%=-I .moke-deps/%)

OCAMLDEP_D=$(OCAMLDEP_WITH_FLAGS)
OCAMLDEP_D_OPT=$(OCAMLDEP_WITH_FLAGS) -native

OCAML_CMI=$(OCAMLC_WITH_FLAGS) -c
OCAML_CMO=$(OCAMLC_WITH_FLAGS) -c
OCAML_CMX=$(OCAMLOPT_WITH_FLAGS) -c
OCAML_CMA=$(OCAMLC_WITH_FLAGS) -a
OCAML_CMXA=$(OCAMLOPT_WITH_FLAGS) -a
OCAMLC_LINK=$(OCAMLC_WITH_FLAGS) -cclib -lparmap_stubs
OCAMLOPT_LINK=$(OCAMLOPT_WITH_FLAGS)
MENHIR_ML=$(MENHIR) --ocamlc "$(OCAMLC_WITH_FLAGS)" --explain --infer
MENHIR_D=$(MENHIR) --ocamldep "$(OCAMLDEP_D)" --depend
MENHIR_D_OPT=$(MENHIR) --ocamlc "$(OCAMLOPT_WITH_FLAGS)" \
	--ocamldep "$(OCAMLDEP_D_OPT)" --depend

CORE_LIBS=unix str bigarray nums dynlink $(PCREDIR:%=pcre)

LIBS_spatch=$(CORE_LIBS) parmap pyml

LIBS_spgen=$(CORE_LIBS)

CORE_MODULES=\
	commons globals parsing_cocci parsing_c
MODULES=\
	$(CORE_MODULES) ctl ocaml python engine popl09 extra

SOURCES_commons=\
	ocamlextra/dumper.ml commands.ml common.ml ograph_simple.ml \
	ograph_extended.ml
SOURCES_globals=\
	config.ml flag.ml iteration.ml $(REGEXP_FILE) regexp.ml
SOURCES_ctl=\
	flag_ctl.ml ast_ctl.ml pretty_print_ctl.ml ctl_engine.ml wrapper_ctl.ml
SOURCES_parsing_cocci=\
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
	parser_cocci_menhir.ml lexer_cocci.ml \
	lexer_cli.ml lexer_script.ml \
	cocci_grep.ml dpll.ml get_constants2.ml id_utils.ml git_grep.ml \
	adjacency.ml commas_on_lists.ml parse_cocci.ml command_line.ml
INTERMEDIATE_parsing_cocci=\
	lexer_cli.ml lexer_cocci.ml lexer_script.ml parser_cocci_menhir.ml \
	parser_cocci_menhir.mli
SOURCES_parsing_c=\
	token_annot.ml flag_parsing_c.ml parsing_stat.ml \
	token_c.ml ast_c.ml includes.ml control_flow_c.ml \
	visitor_c.ml lib_parsing_c.ml control_flow_c_build.ml \
	pretty_print_c.ml semantic_c.ml lexer_parser.ml parser_c.ml lexer_c.ml \
	parse_string_c.ml token_helpers.ml token_views_c.ml \
	cpp_token_c.ml parsing_hacks.ml cpp_analysis_c.ml \
	unparse_cocci.ml parsing_recovery_c.ml parsing_consistency_c.ml \
	danger.ml parse_c.ml unparse_c.ml unparse_hrule.ml \
	type_c.ml cpp_ast_c.ml \
	type_annoter_c.ml comment_annotater_c.ml \
	compare_c.ml test_parsing_c.ml
INTERMEDIATE_parsing_c=\
	lexer_c.ml parser_c.ml parser_c.mli
SOURCES_ocaml=\
	externalanalysis.ml \
	exposed_modules.ml coccilib.ml ocamlcocci_aux.ml $(OCAMLCOCCI_FILE) \
	prepare_ocamlcocci.ml run_ocamlcocci.ml
SOURCES_python=\
	pycocci_aux.ml $(PYCOCCI_FILE) pycocci.ml
SOURCES_engine=\
	flag_matcher.ml lib_engine.ml pretty_print_engine.ml \
	check_exhaustive_pattern.ml \
	check_reachability.ml \
	c_vs_c.ml isomorphisms_c_c.ml \
	cocci_vs_c.ml pattern_c.ml transformation_c.ml \
	asttomember.ml asttoctl2.ml ctltotex.ml \
	postprocess_transinfo.ml ctlcocci_integration.ml
SOURCES_popl09=\
	ast_popl.ml asttopopl.ml insert_quantifiers.ml \
	pretty_print_popl.ml flag_popl.ml popltoctl.ml popl.ml
SOURCES_extra=\
	classic_patch.ml kbuild.ml maintainers.ml

SOURCES_spatch=\
	flag_cocci.ml cocci.ml testing.ml read_options.ml main.ml

SOURCES_spgen=\
	globals.ml ast_tostring.ml detect_patch.ml meta_variable.ml \
	snapshot.ml user_input.ml position_generator.ml disj_generator.ml \
	rule_body.ml rule_header.ml context_rule.ml script_rule.ml \
	file_transform.ml spgen_interactive.ml spgen_lexer.ml spgen_config.ml \
	spgen.ml spgen_test.ml
INTERMEDIATE_SPGEN=\
	spgen_lexer.ml

TOOLS=spatch spgen

PREFIX_spatch=
MODULES_spatch=$(MODULES)

PREFIX_spgen=tools/spgen/source/
MODULES_spgen=$(CORE_MODULES)

SOURCES_ALL=\
	$(foreach module,$(MODULES),$(SOURCES_$(module):%=$(module)/%)) \
	$(foreach tool,$(TOOLS),$(SOURCES_$(tool):%=$(PREFIX_$(tool))%))

SEARCH_PATHS=\
	commons/ocamlextra $(MODULES) $(PREFIX_spgen) $(PCREDIR) $(PYMLDIR) \
	$(PARMAPDIR)

SEARCH_PATH_FLAGS=$(SEARCH_PATHS:%=-I %)

CLEAN_EXT=cmi cmo cmx cmt cmti o d d.opt

ifeq ($(NATIVE),yes)
	TOOLS_SUFFIX=.opt
	DEFAULT_OBJECT=cmx
	OTHER_OBJECT=cmo
	ALL_OBJECTS=cmo cmx
	ifeq ($(filter spatch,$(MAKECMDGOALS)),)
	ifeq ($(filter tools/spgen/source/spgen,$(MAKECMDGOALS)),)
	ifeq ($(filter *.cmo,$(MAKECMDGOALS)),)
		ONLY_NATIVE=yes
	endif
	endif
	endif
else
	TOOLS_SUFFIX=
	DEFAULT_OBJECT=cmo
	OTHER_OBJECT=cmx
	ALL_OBJECTS=cmo
endif

ifeq ($(ONLY_NATIVE),yes)
	DEP_SUFFIX=.d.opt
else
	DEP_SUFFIX=.d
endif

EXPOSED_MODULES:=$(basename \
	$(shell sed -n 's/^.*(\* \(.*\) \*).*$$/\1/p' ocaml/exposed_modules.ml))

COMPILED_EXPOSED_MODULES=\
	$(foreach EXT,cmi $(ALL_OBJECTS),\
		$(patsubst %,ocaml/%.$(EXT),$(notdir $(EXPOSED_MODULES))))

## Global rules

# all makes spatch and spgen

.PHONY: all
all: $(foreach tool,$(TOOLS),$(PREFIX_$(tool))$(tool)$(TOOLS_SUFFIX)) \
	 $(COMPILED_EXPOSED_MODULES)

# clean cleans every module, runs clean-spatch and clean-spgen and
# cleans every bundle.
# The .moke-deps directory contains empty .ml files that replace the files
# generated by ocamllex, ocamlyacc and menhir during ocamldep phase.
# See ocamldep section.

.PHONY: clean
clean: \
		$(foreach module,$(MODULES),clean-$(module)) \
		$(foreach tool,$(TOOLS),clean-$(tool)) \
		clean-bundles
	- rmdir .moke-deps
	rm -f $(COMPILED_EXPOSED_MODULES)

.PHONY: distclean
distclean: clean distclean-bundles
	rm -f Makefile.config configure

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
endef
$(foreach bundle,$(ALL_BUNDLES),$(eval $(foreach_bundle)))

main.cmo: bundles-parmap
main.cmx: bundles-parmap.opt

python/yes_pycocci.cmo: bundles-pyml
python/yes_pycocci.cmx: bundles-pyml.opt

globals/regexp_pcre.cmo: bundles-pcre
globals/regexp_pcre.cmx: bundles-pcre.opt

## Modules

define foreach_module
.PHONY: clean-$(module)
clean-$(module):
	rm -f $(module)/$(module).cmxa \
		$(INTERMEDIATE_$(module):%=$(module)/%) \
		$(INTERMEDIATE_$(module):%=.moke-deps/$(module)/%) \
		$(foreach ext,$(CLEAN_EXT), \
			$(SOURCES_$(module):%.ml=$(module)/%.$(ext)))
	- rmdir .moke-deps/$(module)

$(module)/$(module).cmxa: $(SOURCES_$(module):%.ml=$(module)/%.cmx)
	$(OCAML_CMXA) $$^ -o $$@

$(module)/$(module).cma: $(SOURCES_$(module):%.ml=$(module)/%.cmo)
	$(OCAML_CMA) $$^ -o $$@
endef
$(foreach module,$(MODULES),$(eval $(foreach_module)))

define copy_exposed_module
ocaml/$(notdir $(basename $(module))).$(EXT): $(basename $(module)).$(EXT)
	cp $$< $$@
endef

$(foreach module,$(EXPOSED_MODULES),\
	$(foreach EXT,cmi $(ALL_OBJECTS),$(eval $(copy_exposed_module))))

## Generic compilation rules

%.cmi: %.mli %$(DEP_SUFFIX)
	$(OCAML_CMI) $< -o $@

%.cmo: %.ml %.d
	$(OCAML_CMO) $< -o $@

%.cmx: %.ml %.d.opt
	$(OCAML_CMX) $< -o $@

%.ml: %.mll
	$(OCAMLLEX) $< -o $@

## Parser_c

parsing_c/parser_c.ml: parsing_c/parser_c.mly
	$(OCAMLYACC) $<
parsing_c/parser_c.mli: parsing_c/parser_c.ml

## Parser_cocci_menhir

parsing_cocci/parser_cocci_menhir.d: \
		parsing_cocci/parser_cocci_menhir.mly \
		bundles-menhirLib$(TOOLS_SUFFIX)
	$(MENHIR_D) $< >$@
parsing_cocci/parser_cocci_menhir.d.opt: \
		parsing_cocci/parser_cocci_menhir.mly \
		bundles-menhirLib$(TOOLS_SUFFIX)
#	$(MENHIR_D_OPT) $< >$@
	$(MENHIR_D) $< | sed s/\\.cmo\ /\\.cmx\ /g >$@
parsing_cocci/parser_cocci_menhir.ml: \
		parsing_cocci/parser_cocci_menhir.mly \
		parsing_cocci/parser_cocci_menhir$(DEP_SUFFIX) \
		bundles-menhirLib$(TOOLS_SUFFIX)
	$(MENHIR_ML) $<
parsing_cocci/parser_cocci_menhir.mli: parsing_cocci/parser_cocci_menhir.ml

## Tools: spatch and spgen

define foreach_tool
clean-$(tool):
	rm -f $(PREFIX_$(tool))$(tool) $(PREFIX_$(tool))$(tool).opt \
		$(foreach ext,$(CLEAN_EXT), \
			$(SOURCES_$(tool):%.ml=$(PREFIX_$(tool))%.$(ext)))

$(PREFIX_$(tool))$(tool): \
		$(foreach module,$(MODULES_$(tool)),$(module)/$(module).cma) \
		$(SOURCES_$(tool):%.ml=$(PREFIX_$(tool))%.cmo)
	$(OCAMLC_LINK) $(LIBS_$(tool):=.cma) $$^ -o $$@

$(PREFIX_$(tool))$(tool).opt: \
		$(foreach module,$(MODULES_$(tool)),$(module)/$(module).cmxa) \
		$(SOURCES_$(tool):%.ml=$(PREFIX_$(tool))%.cmx)
	$(OCAMLOPT_LINK) $(LIBS_$(tool):=.cmxa) $$^ -o $$@
endef
$(foreach tool,$(TOOLS),$(eval $(foreach_tool)))

## ocamldep

# .moke-deps is a directory with empty .ml files for units that are
# generated by ocamllex, ocamlyacc and menhir, in order to make them visible
# by ocamldep. The variable OCAMLDEP_WITH_FLAGS includes .mokedep sub-dirs.

.moke-deps:
	mkdir .moke-deps $(MODULES:%=.moke-deps/%)
	touch $(foreach module,$(MODULES),\
		$(INTERMEDIATE_$(module):%=.moke-deps/$(module)/%))

define foreach_source_file

# Dependencies for menhir parser are defined specifically in the
# Parser_cocci_menhir section
ifneq ($(source_file),parsing_cocci/parser_cocci_menhir.ml)

ifeq ($(wildcard $(source_file:.ml=.mli))$(wildcard $(source_file:.ml=.mly)),)
# If there is no .mli files and it will not come by ocamlyacc
$(source_file:.ml=.d): $(source_file) .moke-deps
	$(OCAMLDEP_D) $$< | sed s+\.moke-deps/++g >$$@

$(source_file:.ml=.d.opt): $(source_file) .moke-deps
	$(OCAMLDEP_D_OPT) $$< | sed s+\.moke-deps/++g >$$@

# If there is a unit.ml file has no unit.mli, then unit.cmx depends
# on unit.cmo or unit.cmo depends on unit.cmx (depending which is the
# privileged target) in order to prevent parallel compilations to corrupt
# the unit.cmi file.
$(source_file:.ml=.$(OTHER_OBJECT)): \
	$(source_file:.ml=.$(DEFAULT_OBJECT))

$(source_file:.ml=.cmi): \
	$(source_file:.ml=.$(DEFAULT_OBJECT))
else
# Else there is or will be a .mli file
$(source_file:.ml=.d): $(source_file) $(source_file:.ml=.mli) .moke-deps
	$(OCAMLDEP_D) $$^ | sed s+\.moke-deps/++g >$$@

$(source_file:.ml=.d.opt): $(source_file) $(source_file:.ml=.mli) .moke-deps
	$(OCAMLDEP_D_OPT) $$^ | sed s+\.moke-deps/++g >$$@
endif
endif
endef
$(foreach source_file,$(SOURCES_ALL),$(eval $(foreach_source_file)))

define foreach_source_file_include_dep
	include $(source_file:.ml=$(DEP_SUFFIX))
endef

# Special case for exposed modules, for which we need .cmo files.
define exposed_module_include_dep
	include $(module:=.d)

ifeq ($(wildcard $(module:=.mli)),)
$(module:=.cmi): $(module:=.$(OTHER_OBJECT))
endif
endef

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(MAKECMDGOALS),distclean)
$(foreach source_file,$(SOURCES_ALL), \
	$(eval $(foreach_source_file_include_dep)))

$(foreach module,$(EXPOSED_MODULES), \
	$(eval $(exposed_module_include_dep)))
endif
endif
