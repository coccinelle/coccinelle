.PHONY: all

TEST=test

###############
ORIG = $(TEST:%=%.orig.org)
EDIT = $(TEST:%=%.edit.org)
ORG = $(TEST:%=%.org)
LIST = $(TEST:%=%.list)
CSV = $(TEST:%=%.csv)
BUGS = $(TEST:%=%.bugs)
JGR = $(TEST:%=%.jgr)
PS = $(TEST:%=%.ps)
EPS = $(TEST:%=%.eps)
PDF = $(TEST:%=%.pdf)

.PHONY: all clean

all: test-pdf test-org

test-pdf: clean $(PDF)

test-org:
	../gengraph -p /var/linuxes/linux-2.6. -o test.orig.org

#all: clean $(JGR)

clean:
	rm -f *~
	rm -f $(JGR) $(PS) $(EPS) $(PDF) $(BUGS) $(CSV) $(LIST) $(EDIT)

.SUFFIXES: .csv .jgr .ps .eps .pdf .bugs .org .list

$(JGR): $(CSV) $(BUGS)
	../gengraph -e $(TEST).csv -b $(TEST).bugs > $@

.jgr.ps:
	jgraph -P $< > $@

.jgr.pdf:
	jgraph -P $< | ps2pdf - $@

.ps.eps:
	ps2epsi $< $@

.eps.pdf:
	epstopdf $<

$(EDIT): $(ORIG)
	../scripts/bug_exist.sh test.orig.org 

.org.bugs:
	../scripts/bug_ver.sh $<

.list.csv:
	../scripts/file_exist.sh $<

$(ORG): $(EDIT)

$(BUGS): $(ORG)

$(LIST): $(BUGS)

$(CSV): $(LIST)

$(EPS): $(JGR)

#$(PDF): $(EPS)

$(PDF): $(JGR)

