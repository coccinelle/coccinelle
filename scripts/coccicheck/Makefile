PREFIX?=/tobedefine
PROJECT?=/
SPFLAGS?="-timeout 60 -use_glimpse -D org"

COCCI?=/tobedefine/cocci
RESULTS?=/tobedefine/results

.PHONY:: update viewlog viewfilteredlog viewres checkcocci
.PHONY:: all clean distclean depend

all:
	$(MAKE) -C $(RESULTS)

update:
	./bin/update_result_tree $(PREFIX) $(PROJECT) $(COCCI) $(RESULTS) $(SPFLAGS)

viewlog:
	find $(RESULTS) -name "*.log" | xargs cat | pager

viewfilteredlog:
	find $(RESULTS) -name "*.log" | xargs cat | \
		grep -ve "^$$" | \
		grep -v "HANDLING" | \
		grep -v "NOTE" | \
		grep -v "Unsupported form of #define" | \
		grep -v "no glimpse keyword inferred from snippet" | \
		grep -v "EXN:Common.Timeout" | \
		grep -v "FLOW: can't find exit or error exit:" | \
		grep -v "FLOW: deadcode detected:" | \
		grep -v "LEXER: identifier with dollar:" | \
		grep -v "LEXER: unrecognised symbol in char:" | \
		grep -v "LEXER: ZARB" | \
		grep -v "CFG: orphelin nodes, maybe something weird happened" | \
		grep -v "not worth trying" | \
		grep -v "MultiFound field:" | \
		grep -ve "(ONCE) warning: I consider .* as a constant" | \
		grep -ve "TYPE: header .* not found"

viewres:
	find $(RESULTS) -name "*.out" | xargs pager

checkcocci:
	for c in `find $(COCCI) -name "*.cocci"`; do spatch -parse_cocci $$c > /dev/null ; done

clean:
	find $(RESULTS) -name "*.out" -delete
	find $(RESULTS) -name "*.log" -delete

distclean:
	rm -rf $(RESULTS)

depend: update
