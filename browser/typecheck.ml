(*************************************************************************)
(*                                                                       *)
(*                         OCaml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open StdLabels
open Tk
open Parsetree
open Typedtree
open Location
open Jg_tk
open Mytypes

(* Optionally preprocess a source file *)

let preprocess ~pp ~ext text =
  let sourcefile = Filename.temp_file "caml" ext in
  begin try
    let oc = open_out_bin sourcefile in
    output_string oc text;
    flush oc;
    close_out oc
  with _ ->
    failwith "Preprocessing error"
  end;
  let tmpfile = Filename.temp_file "camlpp" ext in
  let comm = Printf.sprintf "%s %s > %s" pp sourcefile tmpfile in
  if Ccomp.command comm <> 0 then begin
    Sys.remove sourcefile;
    Sys.remove tmpfile;
    failwith "Preprocessing error"
  end;
  Sys.remove sourcefile;
  tmpfile

exception Outdated_version

let parse_pp ~parse ~wrap ~ext text =
  Location.input_name := "";
  match !Clflags.preprocessor with
    None ->
      let buffer = Lexing.from_string text in
      Location.init buffer "";
      parse buffer
  | Some pp ->
      let tmpfile = preprocess ~pp ~ext text in
      let ast_magic =
        if ext = ".ml" then Config.ast_impl_magic_number
        else Config.ast_intf_magic_number in
      let ic = open_in_bin tmpfile in
      let ast =
        try
          let buffer = really_input_string ic (String.length ast_magic) in
          if buffer = ast_magic then begin
            ignore (input_value ic);
            wrap (input_value ic)
          end else if String.sub buffer ~pos:0 ~len:9
                    = String.sub ast_magic ~pos:0 ~len:9
          then raise Outdated_version
          else raise Exit
        with
          Outdated_version ->
            close_in ic;
            Sys.remove tmpfile;
            failwith "OCaml and preprocessor have incompatible versions"
        | _ ->
            seek_in ic 0;
            let buffer = Lexing.from_channel ic in
            Location.init buffer "";
            parse buffer
      in
      close_in ic;
      Sys.remove tmpfile;
      ast

let update_type_info txt =
  let open Cmt2annot_raw in
  let iter = iterator true ~scope:(Location.in_file txt.name) in
  List.iter ~f:(binary_part iter) (Cmt_format.get_saved_types ());
  txt.type_info <- Stypes.get_info ()

let nowarnings = ref false

let f txt =
  let error_messages = ref [] in
  let text = Jg_text.get_all txt.tw
  and env = ref (Compmisc.initial_env ()) in
  let tl, ew, end_message =
    Jg_message.formatted ~title:"Warnings" ~ppf:Format.err_formatter () in
  Text.tag_remove txt.tw ~tag:"error" ~start:tstart ~stop:tend;
  txt.structure <- [];
  txt.type_info <- [];
  txt.signature <- [];
  txt.psignature <- [];
  ignore (Stypes.get_info ());
  Clflags.annotations := true;
  Clflags.color := Some Misc.Color.Never;

  begin try

    if Filename.check_suffix txt.name ".mli" then
    let psign = parse_pp text ~ext:".mli"
        ~parse:Parse.interface ~wrap:(fun x -> x) in
    txt.psignature <- psign;
    txt.signature <- (Typemod.type_interface !env psign).sig_type;

    else (* others are interpreted as .ml *)

    let psl = parse_pp text ~ext:".ml"
        ~parse:Parse.use_file ~wrap:(fun x -> [Parsetree.Ptop_def x]) in
    List.iter psl ~f:
    begin function
      Ptop_def pstr ->
        let str, sign, _names, _, env' = Typemod.type_structure !env pstr in
        txt.structure <- txt.structure @ str.str_items;
        txt.signature <- txt.signature @ sign;
        env := env'
    | Ptop_dir _ -> ()
    end;
    update_type_info txt

  with
    Lexer.Error _ | Syntaxerr.Error _
  | Typecore.Error _ | Typemod.Error _
  | Typeclass.Error _ | Typedecl.Error _
  | Typetexp.Error _ | Includemod.Error _
  | Persistent_env.Error _ | Env.Error _
  | Ctype.Tags _ | Failure _ as exn ->
      update_type_info txt;
      let et, ew, end_message = Jg_message.formatted ~title:"Error !" () in
      error_messages := et :: !error_messages;
      let range = match exn with
        Lexer.Error (err, l) -> l
      | Syntaxerr.Error err -> Syntaxerr.location_of_error err
      | Typecore.Error (l, env, err) -> l
      | Typeclass.Error (l, env, err) -> l
      | Typedecl.Error (l, err) -> l
      | Typemod.Error (l, env, err) -> l
      | Typetexp.Error (l, env, err) -> l
      | Env.Error (Missing_module (l, _, _) | Illegal_value_name (l, _) |
                   Lookup_error (l, _, _)) -> l
      | _ -> Location.none
      in
      begin match exn with
      | Cmi_format.Error err ->
          Cmi_format.report_error Format.std_formatter err
      | Ctype.Tags(l, l') ->
          Format.printf 
            "In this program,@ variant constructors@ `%s and `%s@ %s.@."
            l l' "have same hash value"
      | Failure s ->
          Format.printf "%s.@." s
      | _ -> Location.report_exception Format.std_formatter exn
      end;
      end_message ();
      let s = range.loc_start.Lexing.pos_cnum in
      let e = range.loc_end.Lexing.pos_cnum in
      if s < e then
        Jg_text.tag_and_see txt.tw ~start:(tpos s) ~stop:(tpos e) ~tag:"error"
  end;
  end_message ();
  if !nowarnings || Text.index ew ~index:tend = `Linechar (2,0)
  then destroy tl
  else begin
    error_messages := tl :: !error_messages;
    Text.configure ew ~state:`Disabled;
    bind ew ~events:[`Modified([`Double], `ButtonReleaseDetail 1)]
      ~action:(fun _ ->
        try
          let start, ende = Text.tag_nextrange ew ~tag:"sel" ~start:(tpos 0) in
          let s = Text.get ew ~start:(start,[]) ~stop:(ende,[]) in
          let n = int_of_string s in
          Text.mark_set txt.tw ~index:(tpos n) ~mark:"insert";
          Text.see txt.tw ~index:(`Mark "insert", [])
        with _ -> ())
  end;
  !error_messages
