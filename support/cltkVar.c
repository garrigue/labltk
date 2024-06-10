/***********************************************************************/
/*                                                                     */
/*                 MLTk, Tcl/Tk interface of OCaml                     */
/*                                                                     */
/*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    */
/*               projet Cristal, INRIA Rocquencourt                    */
/*            Jacques Garrigue, Kyoto University RIMS                  */
/*                                                                     */
/*  Copyright 2002 Institut National de Recherche en Informatique et   */
/*  en Automatique and Kyoto University.  All rights reserved.         */
/*  This file is distributed under the terms of the GNU Library        */
/*  General Public License, with the special exception on linking      */
/*  described in file LICENSE found in the OCaml source tree.          */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

/* Alternative to tkwait variable */
#include <string.h>
#include <tcl.h>
#include <tk.h>
#include <mlvalues.h>
#include <memory.h>
#include <alloc.h>
#include <callback.h>
#include "camltk.h"

CAMLprim value camltk_getvar(value var)
{
  char *s;
  char *stable_var = NULL;
  CheckInit();

  stable_var = string_to_c(var);
  s = (char *)Tcl_GetVar(cltclinterp,stable_var,
                         TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG);
  caml_stat_free(stable_var);

  if (s == NULL)
    tk_error(Tcl_GetStringResult(cltclinterp));
  else
    return(tcl_string_to_caml(s));
}

CAMLprim value camltk_setvar(value var, value contents)
{
  char *s;
  char *stable_var = NULL;
  char *utf_contents;
  CheckInit();

  /* SetVar makes a copy of the contents. */
  /* In case we have write traces in OCaml, it's better to make sure that
     var doesn't move... */
  stable_var = string_to_c(var);
  utf_contents = caml_string_to_tcl(contents);
  s = (char *)Tcl_SetVar(cltclinterp,stable_var, utf_contents,
                         TCL_GLOBAL_ONLY|TCL_LEAVE_ERR_MSG);
  caml_stat_free(stable_var);
  if( s == utf_contents ){
    tk_error("camltk_setvar: Tcl_SetVar returned strange result. Call the author of mlTk!");
  }
  caml_stat_free(utf_contents);

  if (s == NULL)
    tk_error(Tcl_GetStringResult(cltclinterp));
  else
    return(Val_unit);
}


/* The appropriate type is
typedef char *(Tcl_VarTraceProc) _ANSI_ARGS_((ClientData clientData,
        Tcl_Interp *interp, char *part1, char *part2, int flags));
 */
static char * tracevar(ClientData clientdata, Tcl_Interp *interp,
                char *name1, char *name2, int flags)
     /* interp -> Interpreter containing variable. */
     /* name1  -> Name of variable. */
     /* name2  -> Second part of variable name. */
     /* flags  -> Information about what happened. */
{
  Tcl_UntraceVar2(interp, name1, name2,
                TCL_GLOBAL_ONLY|TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
                tracevar, clientdata);
  caml_callback2(*handler_code,Val_int(clientdata),Val_unit);
  return (char *)NULL;
}

/* Sets up a callback upon modification of a variable */
CAMLprim value camltk_trace_var(value var, value cbid)
{
  char *cvar = NULL;

  CheckInit();
  /* Make a copy of var, since Tcl will modify it in place, and we
   * don't trust that much what it will do here
   */
  cvar = string_to_c(var);
  if (Tcl_TraceVar(cltclinterp, cvar,
                   TCL_GLOBAL_ONLY|TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
                   tracevar,
                   (ClientData) (Long_val(cbid)))
                   != TCL_OK) {
    caml_stat_free(cvar);
    tk_error(Tcl_GetStringResult(cltclinterp));
  };
  caml_stat_free(cvar);
  return Val_unit;
}

CAMLprim value camltk_untrace_var(value var, value cbid)
{
  char *cvar = NULL;

  CheckInit();
  /* Make a copy of var, since Tcl will modify it in place, and we
   * don't trust that much what it will do here
   */
  cvar = string_to_c(var);
  Tcl_UntraceVar(cltclinterp, cvar,
                 TCL_GLOBAL_ONLY|TCL_TRACE_WRITES|TCL_TRACE_UNSETS,
                 tracevar,
                 (ClientData) (Long_val(cbid)));
  caml_stat_free(cvar);
  return Val_unit;
}
