#######################################################################
#                                                                     #
#                 MLTk, Tcl/Tk interface of OCaml                     #
#                                                                     #
#    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    #
#               projet Cristal, INRIA Rocquencourt                    #
#            Jacques Garrigue, Kyoto University RIMS                  #
#                                                                     #
#  Copyright 1999 Institut National de Recherche en Informatique et   #
#  en Automatique and Kyoto University.  All rights reserved.         #
#  This file is distributed under the terms of the GNU Library        #
#  General Public License, with the special exception on linking      #
#  described in file LICENSE found in the OCaml source tree.          #
#                                                                     #
#######################################################################

# Top Makefile for mlTk

SUBDIRS=compiler support lib jpf frx examples_labltk \
	examples_camltk browser
SUBDIRS_GENERATED=camltk labltk
WARNERR=-warn-error A-3
include config/Makefile

all-devel:
	$(MAKE) all opt WARNERR="$(WARNERR)"
all: library
	cd browser; $(MAKE)
opt: libraryopt

library:
	cd support; $(MAKE)
	cd compiler; $(MAKE)
	cd labltk; $(MAKE) -f Makefile.gen
	cd labltk; $(MAKE)
	cd camltk; $(MAKE) -f Makefile.gen
	cd camltk; $(MAKE)
	cd lib; $(MAKE)
	cd jpf; $(MAKE)
	cd frx; $(MAKE)

libraryopt:
	cd support; $(MAKE) opt
	cd labltk; $(MAKE) -f Makefile.gen
	cd labltk; $(MAKE) opt
	cd camltk; $(MAKE) -f Makefile.gen
	cd camltk; $(MAKE) opt
	cd lib; $(MAKE) opt
	cd jpf; $(MAKE) opt
	cd frx; $(MAKE) opt

byte: all
opt: allopt

.PHONY: all allopt byte opt apiref library libraryopt
.PHONY: labltk camltk examples examples_labltk examples_camltk
.PHONY: install installopt partialclean clean depend

labltk: Widgets.src
	compiler/tkcompiler -outdir labltk
	cd labltk; $(MAKE)

camltk: Widgets.src
	compiler/tkcompiler -camltk -outdir camltk
	cd camltk; $(MAKE)

examples: examples_labltk examples_camltk

examples_labltk:
	cd examples_labltk; $(MAKE) all

examples_camltk:
	cd examples_camltk; $(MAKE) all

SUPPORTMLIS= fileevent support textvariable timer tkthread widget
apiref:
	$(BINDIR)/ocamldoc -I +threads -I support -I labltk $(SUPPORTMLIS:%=support/%.mli) labltk/*.mli labltk/tk.ml -sort -d htdocs/apiref -html || echo "There were errors"

install:
	cd support; $(MAKE) install
	cd lib; $(MAKE) install
	cd labltk; $(MAKE) install
	cd camltk; $(MAKE) install
	cd compiler; $(MAKE) install
	cd jpf; $(MAKE) install
	cd frx; $(MAKE) install
	cd browser; $(MAKE) install
	if test -f lib/labltk.cmxa; then $(MAKE) installopt; else :; fi

install-browser:
	cd browser; $(MAKE) install

installopt:
	cd support; $(MAKE) installopt
	cd lib; $(MAKE) installopt
	cd labltk; $(MAKE) installopt
	cd camltk; $(MAKE) installopt
	cd jpf; $(MAKE) installopt
	cd frx; $(MAKE) installopt

uninstall:
	ocamlfind remove labltk
	rm -f $(INSTALLBINDIR)/labltk
	rm -f $(INSTALLBINDIR)/ocamlbrowser$(EXE)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

partialclean clean:
	for d in $(SUBDIRS); do \
	    cd $$d; $(MAKE) -f Makefile clean; cd ..; \
	done
	for d in $(SUBDIRS_GENERATED); do \
	    cd $$d; $(MAKE) -f Makefile.gen clean; cd ..; \
	done

depend:
