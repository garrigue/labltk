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

include Makefile.common

all: support.cmo rawwidget.cmo widget.cmo protocol.cmo \
     textvariable.cmo timer.cmo fileevent.cmo camltkwrap.cmo \
     tkthread.cmo lib$(LIBNAME).$(A)

opt: support.cmx rawwidget.cmx widget.cmx protocol.cmx \
     textvariable.cmx timer.cmx fileevent.cmx camltkwrap.cmx \
     tkthread.cmx lib$(LIBNAME).$(A)

COBJS=cltkCaml.$(O) cltkUtf.$(O) cltkEval.$(O) cltkEvent.$(O) \
      cltkFile.$(O) cltkMain.$(O) cltkMisc.$(O) cltkTimer.$(O) \
      cltkVar.$(O) cltkWait.$(O) cltkImg.$(O)

CCFLAGS=-I$(LIBDIR)/caml $(TK_DEFS) $(SHAREDCCCOMPOPTS)

COMPFLAGS=-I +unix
THFLAGS=-I +threads -I vmthreads
TKLDOPTS=$(TK_LINK:%=-ldopt "%")

lib$(LIBNAME).$(A): $(COBJS)
	$(MKLIB) -o $(LIBNAME) $(COBJS) $(TKLDOPTS)

PUBMLI=fileevent.mli protocol.mli textvariable.mli timer.mli \
    rawwidget.mli widget.mli
PUB= $(PUBMLI) $(PUBMLI:.mli=.cmi) tkthread.mli tkthread.cmi tkthread.cmo \
	camltkwrap.cmi
PUBX= $(PUBMLI:.mli=.cmx) camltkwrap.cmx

ifeq ($(USE_FINDLIB),yes)
install:
	ocamlfind install labltk META $(PUB) lib$(LIBNAME).$(A) \
	  -optional dll$(LIBNAME)$(EXT_DLL)
	$(RANLIB) lib$(LIBNAME).$(A)

installopt:
	ocamlfind install labltk -add $(PUBX) \
	  -optional tkthread.cmx tkthread.$(O)
else
install:
	if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(PUB) lib$(LIBNAME).$(A) $(INSTALLDIR)
	cd $(INSTALLDIR); $(RANLIB) lib$(LIBNAME).$(A)
	cd $(INSTALLDIR); chmod 644 $(PUB) lib$(LIBNAME).$(A)
	if test -f dll$(LIBNAME)$(EXT_DLL); then \
	   cp dll$(LIBNAME)$(EXT_DLL) $(STUBLIBDIR)/; fi

installopt:
	@if test -d $(INSTALLDIR); then : ; else mkdir $(INSTALLDIR); fi
	cp $(PUBX) $(INSTALLDIR)
	if test -f tkthread.$(O); then \
	  cp tkthread.cmx tkthread.$(O) $(INSTALLDIR); \
	  chmod 644 $(INSTALLDIR)/tkthread.cmx $(INSTALLDIR)/tkthread.$(O); \
	fi
endif

clean:
	rm -f *.cm* *.o *.a *.so *.obj *.lib *.dll *.exp

.SUFFIXES:
.SUFFIXES: .mli .ml .cmi .cmo .cmx .mlp .c .$(O)

.mli.cmi:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmo:
	$(CAMLCOMP) $(COMPFLAGS) $<

.ml.cmx:
	$(CAMLOPT) -c $(COMPFLAGS) $<

.c.$(O):
	$(CC) $(BYTECCCOMPOPTS) $(OCAMLC_CFLAGS) $(CCFLAGS) -c $<

tkthread.cmi: tkthread.mli
	$(CAMLCOMP) $(COMPFLAGS) $(THFLAGS) $<
tkthread.cmo: tkthread.ml
	$(CAMLCOMP) $(COMPFLAGS) $(THFLAGS) $<
tkthread.cmx: tkthread.ml
	if test -f $(LIBDIR)/systhreads/threads.cmxa; then \
	  $(CAMLOPT) -c $(COMPFLAGS) $(THFLAGS) $< ; \
	fi

depend:
	$(CAMLDEP) *.mli *.ml > .depend

$(COBJS): camltk.h

include .depend
