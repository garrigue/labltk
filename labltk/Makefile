#######################################################################
#                                                                     #
#                 MLTk, Tcl/Tk interface of OCaml                     #
#                                                                     #
#    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    #
#               projet Cristal, INRIA Rocquencourt                    #
#            Jacques Garrigue, Kyoto University RIMS                  #
#                                                                     #
#  Copyright 2002 Institut National de Recherche en Informatique et   #
#  en Automatique and Kyoto University.  All rights reserved.         #
#  This file is distributed under the terms of the GNU Library        #
#  General Public License, with the special exception on linking      #
#  described in file LICENSE found in the OCaml source tree.          #
#                                                                     #
#######################################################################

include ../support/Makefile.common

COMPFLAGS= -I ../support -no-alias-deps -I +unix

all: labltkobjs

opt: labltkobjsx

include ./modules

LABLTKOBJS = $(WIDGETOBJS) tk.cmo labltk.cmo
LABLTKOBJSX = $(LABLTKOBJS:.cmo=.cmx)

labltkobjs: $(LABLTKOBJS)

labltkobjsx: $(LABLTKOBJSX)

ifeq ($(USE_FINDLIB),yes)
install:
	ocamlfind install labltk -add \
	  $(LABLTKOBJS:.cmo=.cmi) $(WIDGETOBJS:.cmo=.mli)
installopt:
	ocamlfind install labltk -add $(LABLTKOBJSX)
else
install:
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(LABLTKOBJS:.cmo=.cmi) $(WIDGETOBJS:.cmo=.mli) $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/*.cmi

installopt:
	@if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(LABLTKOBJSX) $(INSTALLDIR)
	chmod 644 $(INSTALLDIR)/*.cmx
endif

clean:
	$(MAKE) -f Makefile.gen clean

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .mlp

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

include .depend
