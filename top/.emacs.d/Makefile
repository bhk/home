##############################
# Defines
##############################

ifeq ($(shell uname),Darwin)
  EMACSPRE = unset EMACSLOADPATH && # works around Carbon emacs problem
else
  CURDIR := $(shell cygpath -m `pwd`)
endif

EMACS = emacs
RUNEMACS = $(EMACSPRE) $(EMACS) -batch -eval '(push "$(CURDIR)" load-path)'


##############################
# Project description
##############################


ELISPS = fontsize.el tree.el p4utils.el init.el lua-mode.el js2.el
QTESTS = tree_q.el


##############################
# Rules
##############################

ELCS = $(patsubst %.el,%.elc,$(ELISPS))
OKS = $(patsubst %.el,%.ok,$(QTESTS))

.PHONY: all test compile clean

all: compile test

test: $(ELCS)

compile: $(OKS)

clean:
	rm -rf *.elc *.ok

%_q.ok: %_q.el %.el %_q.elc %.elc
	$(RUNEMACS) -l $< && touch $@

%.elc: %.el
	$(RUNEMACS) -eval '(byte-compile-file "$^")'

%.elc: %.el.gz
	$(RUNEMACS) -eval '(byte-compile-file "$^")'

echo:
	@echo "$(ECHOVAR)"

