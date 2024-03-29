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

COMPFLAGS=-I ../lib -I ../labltk -I ../support -w s -dllpath ../support

all: hello demo eyes calc clock tetris lang

opt: hello.opt demo.opt eyes.opt calc.opt clock.opt tetris.opt

hello: hello.cmo
	$(CAMLC) $(COMPFLAGS) -o hello $(LIBNAME).cma hello.cmo

demo: demo.cmo
	$(CAMLC) $(COMPFLAGS) -o demo $(LIBNAME).cma demo.cmo

eyes: eyes.cmo
	$(CAMLC) $(COMPFLAGS) -o eyes $(LIBNAME).cma eyes.cmo

calc: calc.cmo
	$(CAMLC) $(COMPFLAGS) -o calc $(LIBNAME).cma calc.cmo

clock: clock.cmo
	$(CAMLC) $(COMPFLAGS) -o clock $(LIBNAME).cma \
	  -I +unix unix.cma clock.cmo

clock.opt: clock.cmx
	$(CAMLOPT) $(COMPFLAGS) -o clock.opt \
	      $(LIBNAME).cmxa -I +unix unix.cmxa clock.cmx

tetris: tetris.cmo
	$(CAMLC) $(COMPFLAGS) -o tetris $(LIBNAME).cma tetris.cmo

taquin: taquin.cmo
	$(CAMLC) $(COMPFLAGS) -o taquin $(LIBNAME).cma taquin.cmo

lang: lang.cmo
	$(CAMLC) $(COMPFLAGS) -o lang $(LIBNAME).cma lang.cmo

clean:
	rm -f hello demo eyes calc clock tetris lang *.opt *.o *.cm*

.SUFFIXES :
.SUFFIXES : .mli .ml .cmi .cmx .cmo .opt

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.cmx.opt:
	$(CAMLOPT) $(COMPFLAGS) -o $@ $(LIBNAME).cmxa $<
