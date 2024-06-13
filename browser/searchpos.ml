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

open Asttypes
open StdLabels
open Support
open Tk
open Jg_tk
open Parsetree
open Typedtree
open Types
open Location
open Longident
open Path
open Env
open Searchid

(* auxiliary functions *)

let (~!) = Jg_memo.fast ~f:Str.regexp

let lines_to_chars n ~text:s =
  let l = String.length s in
  let rec ltc n ~pos =
    if n = 1 || pos >= l then pos else
    if s.[pos] = '\n' then ltc (n-1) ~pos:(pos+1) else ltc n ~pos:(pos+1)
  in ltc n ~pos:0

let in_loc loc ~pos =
  loc.loc_ghost || pos >= loc.loc_start.Lexing.pos_cnum
                   && pos < loc.loc_end.Lexing.pos_cnum

let le_loc loc1 loc2 =
  loc1.loc_start.Lexing.pos_cnum <= loc2.loc_start.Lexing.pos_cnum
  && loc1.loc_end.Lexing.pos_cnum >= loc2.loc_end.Lexing.pos_cnum

let add_found ~found sol ~env ~loc =
  if loc.loc_ghost then () else
  if List.exists !found ~f:(fun (_,_,loc') -> le_loc loc loc') then ()
  else found := (sol, env, loc) ::
    List.filter !found ~f:(fun (_,_,loc') -> not (le_loc loc' loc))

let observe ~ref ?init f x =
  let old = !ref in
  begin match init with None -> () | Some x -> ref := x end;
  try (f x : unit); let v = !ref in ref := old; v
  with exn -> ref := old; raise exn

let rec string_of_longident = function
    Lident s -> s
  | Ldot (id,s) -> string_of_longident id ^ "." ^ s
  | Lapply (id1, id2) ->
      string_of_longident id1 ^ "(" ^ string_of_longident id2 ^ ")"

let string_of_path p = string_of_longident (Searchid.longident_of_path p)

let rec parent_path = function
    Pdot (path, _) -> Some path
  | Pident _ | Papply _ -> None
  | Pextra_ty (path, Pcstr_ty _) -> Some path
  | Pextra_ty (path, Pext_ty) -> parent_path path

let rec ident_of_path ~default = function
    Pident i -> i
  | Pdot (_, s) | Pextra_ty (_, Pcstr_ty s) -> Ident.create_local s
  | Papply _ -> Ident.create_local default
  | Pextra_ty (path, Pext_ty) -> ident_of_path ~default path

let rec head_id = function
    Pident id -> id
  | Pdot (path,_) | Pextra_ty (path,_) -> head_id path
  | Papply (path,_) -> head_id path (* wrong, but ... *)

let rec list_of_path = function
    Pident id -> [Ident.name id]
  | Pdot (path, s) | Pextra_ty (path, Pcstr_ty s) -> list_of_path path @ [s]
  | Papply (path, _) -> list_of_path path (* wrong, but ... *)
  | Pextra_ty (path, Pext_ty) -> list_of_path path

(* a simple wrapper *)

class buffer ~size = object
  val buffer = Buffer.create size
  method out buf = Buffer.add_substring buffer buf
  method get = Buffer.contents buffer
end

(* Search in a signature *)

type skind = [`Type|`Class|`Module|`Modtype]

let found_sig = ref ([] : ((skind * Longident.t) * Env.t * Location.t) list)
let add_found_sig = add_found ~found:found_sig

let rec search_pos_type t ~pos ~env =
  if in_loc ~pos t.ptyp_loc then
  begin match t.ptyp_desc with
    Ptyp_any
  | Ptyp_var _ -> ()
  | Ptyp_variant(tl, _, _) ->
      List.iter tl ~f:
        begin fun prf -> match prf.prf_desc with
            Rtag (_,_,tl) -> List.iter tl ~f:(search_pos_type ~pos ~env)
          | Rinherit st -> search_pos_type ~pos ~env st
        end
  | Ptyp_arrow (_, t1, t2) ->
      search_pos_type t1 ~pos ~env;
      search_pos_type t2 ~pos ~env
  | Ptyp_tuple tl ->
      List.iter tl ~f:(search_pos_type ~pos ~env)
  | Ptyp_constr (lid, tl) ->
      List.iter tl ~f:(search_pos_type ~pos ~env);
      add_found_sig (`Type, lid.txt) ~env ~loc:t.ptyp_loc
  | Ptyp_object (fl, _) ->
      List.iter fl ~f:
        (fun pof -> match pof.pof_desc with
          Oinherit ty | Otag (_, ty) -> search_pos_type ty ~pos ~env)
  | Ptyp_class (lid, tl) ->
      List.iter tl ~f:(search_pos_type ~pos ~env);
      add_found_sig (`Type, lid.txt) ~env ~loc:t.ptyp_loc
  | Ptyp_alias (t, _)
  | Ptyp_poly (_, t) -> search_pos_type ~pos ~env t
  | Ptyp_package (_, stl) ->
     List.iter stl ~f:(fun (_, ty) -> search_pos_type ty ~pos ~env)
  | Ptyp_open (_, ct) ->
      search_pos_type ct ~pos ~env
  | Ptyp_extension _ -> ()
  end

let rec search_pos_class_type cl ~pos ~env =
  if in_loc cl.pcty_loc ~pos then
    begin match cl.pcty_desc with
      Pcty_constr (lid, _) ->
        add_found_sig (`Class, lid.txt) ~env ~loc:cl.pcty_loc
    | Pcty_signature  cl ->
        List.iter cl.pcsig_fields ~f: (fun fl ->
          begin match fl.pctf_desc with
              Pctf_inherit cty -> search_pos_class_type cty ~pos ~env
            | Pctf_val (_, _, _, ty)
            | Pctf_method (_, _, _, ty) ->
                if in_loc fl.pctf_loc ~pos then search_pos_type ty ~pos ~env
            | Pctf_constraint (ty1, ty2) ->
                if in_loc fl.pctf_loc ~pos then begin
                  search_pos_type ty1 ~pos ~env;
                  search_pos_type ty2 ~pos ~env
                end
	    | Pctf_attribute _
            | Pctf_extension _ -> ()
          end)
    | Pcty_arrow (_, ty, cty) ->
        search_pos_type ty ~pos ~env;
        search_pos_class_type cty ~pos ~env
    | Pcty_extension _ -> ()
    | Pcty_open (_, cty) ->
        search_pos_class_type cty ~pos ~env
    end

let search_pos_arguments ~pos ~env = function
    Pcstr_tuple l -> List.iter l ~f:(search_pos_type ~pos ~env)
  | Pcstr_record l -> List.iter l ~f:(fun ld -> search_pos_type ld.pld_type ~pos ~env)

let search_pos_constructor pcd ~pos ~env =
  if in_loc ~pos pcd.pcd_loc then begin
    Stdlib.Option.iter (search_pos_type ~pos ~env) pcd.pcd_res;
    search_pos_arguments ~pos ~env pcd.pcd_args
  end

let search_pos_type_decl td ~pos ~env =
  if in_loc ~pos td.ptype_loc then begin
    begin match td.ptype_manifest with
      Some t -> search_pos_type t ~pos ~env
    | None -> ()
    end;
    let rec search_tkind = function
      Ptype_abstract
    | Ptype_open -> ()
    | Ptype_variant dl ->
        List.iter dl ~f:(search_pos_constructor ~pos ~env)
    | Ptype_record dl ->
        List.iter dl ~f:(fun pld -> search_pos_type pld.pld_type ~pos ~env) in
    search_tkind td.ptype_kind;
    List.iter td.ptype_cstrs ~f:
      begin fun (t1, t2, _) ->
        search_pos_type t1 ~pos ~env;
        search_pos_type t2 ~pos ~env
      end
  end

let search_pos_extension ext ~pos ~env =
  begin match ext.pext_kind with
    Pext_decl (_, l, _) -> search_pos_arguments l ~pos ~env
  | Pext_rebind _ -> ()
  end
  
let rec search_pos_signature l ~pos ~env =
  ignore (
  List.fold_left l ~init:env ~f:
  begin fun env pt ->
    let env = match pt.psig_desc with
      Psig_open {popen_override=ovf; popen_expr=id} ->
        let path, mt = lookup_module ~loc:Location.none id.txt env in
        begin match open_signature ovf path env with
          Ok env -> env
        | Error _ -> env
        end
    | sign_item ->
        try add_signature (Typemod.transl_signature env [pt]).sig_type env
        with Typemod.Error _ | Typeclass.Error _
        | Typetexp.Error _  | Typedecl.Error _ -> env
    in
    if in_loc ~pos pt.psig_loc then
      begin match pt.psig_desc with
        Psig_value desc -> search_pos_type desc.pval_type ~pos ~env
      | Psig_type (_, l) ->
          List.iter l ~f:(search_pos_type_decl ~pos ~env)
      | Psig_typext pty ->
	  List.iter pty.ptyext_constructors
	    ~f:(search_pos_extension ~pos ~env);
	  add_found_sig (`Type, pty.ptyext_path.txt) ~env ~loc:pt.psig_loc
      | Psig_exception ext ->
	  search_pos_extension ext.ptyexn_constructor ~pos ~env;
	  add_found_sig (`Type, Lident "exn") ~env ~loc:pt.psig_loc
      | Psig_module pmd ->
          search_pos_module pmd.pmd_type ~pos ~env
      | Psig_recmodule decls ->
          List.iter decls ~f:(fun pmd -> search_pos_module pmd.pmd_type ~pos ~env)
      | Psig_modtype {pmtd_type=Some t} ->
          search_pos_module t ~pos ~env
      | Psig_modtype _ -> ()
      | Psig_class l ->
          List.iter l
            ~f:(fun ci -> search_pos_class_type ci.pci_expr ~pos ~env)
      | Psig_class_type l ->
          List.iter l
            ~f:(fun ci -> search_pos_class_type ci.pci_expr ~pos ~env)
      (* The last cases should not happen in generated interfaces *)
      | Psig_open {popen_expr=lid} ->
        add_found_sig (`Module, lid.txt) ~env ~loc:pt.psig_loc
      | Psig_include {pincl_mod=t} -> search_pos_module t ~pos ~env
      | Psig_attribute _ | Psig_extension _ -> ()
      | Psig_typesubst _ | Psig_modsubst _ | Psig_modtypesubst _ -> ()
      end;
    env
  end)

and search_pos_module m ~pos ~env =
  if in_loc m.pmty_loc ~pos then begin
    begin match m.pmty_desc with
      Pmty_ident lid -> add_found_sig (`Modtype, lid.txt) ~env ~loc:m.pmty_loc
    | Pmty_alias lid -> add_found_sig (`Module, lid.txt) ~env ~loc:m.pmty_loc
    | Pmty_signature sg -> search_pos_signature sg ~pos ~env
    | Pmty_functor (pm1, m2) ->
        begin match pm1 with
        | Unit -> ()
        | Named (_, m1) -> search_pos_module ~pos ~env m1
        end;
        search_pos_module m2 ~pos ~env
    | Pmty_with (m, l) ->
        search_pos_module m ~pos ~env;
        List.iter l ~f:
          begin function
              Pwith_type (_, t) -> search_pos_type_decl t ~pos ~env
            | _ -> ()
          end
    | Pmty_typeof md ->
        ()   (* TODO? *)
    | Pmty_extension _ -> ()
    end
  end

let search_pos_signature l ~pos ~env =
  observe ~ref:found_sig (search_pos_signature ~pos ~env) l

(* the module display machinery *)

type module_widgets =
    { mw_frame: Widget.frame Widget.widget;
      mw_title: Widget.label Widget.widget option;
      mw_detach: Widget.button Widget.widget;
      mw_edit: Widget.button Widget.widget;
      mw_intf: Widget.button Widget.widget }

let shown_modules = Hashtbl.create 17
let default_frame = ref None
let set_path = ref (fun _ ~sign -> assert false)
let filter_modules () =
  Hashtbl.iter
    (fun key data ->
      if not (Winfo.exists data.mw_frame) then
        Hashtbl.remove shown_modules key)
    shown_modules
let add_shown_module path ~widgets =
  Hashtbl.add shown_modules path widgets
let find_shown_module path =
  try
    filter_modules ();
    Hashtbl.find shown_modules path
  with Not_found ->
    match !default_frame with
      None -> raise Not_found
    | Some mw -> mw

let is_shown_module path =
  !default_frame <> None ||
  (filter_modules (); Hashtbl.mem shown_modules path)

(* Viewing a signature *)

(* Forward definitions of Viewer.view_defined and Editor.editor *)
let view_defined_ref = ref (fun lid ~env -> ())
let editor_ref = ref (fun ?file ?pos ?opendialog () -> ())

let edit_source ~file ~path ~sign =
  match sign with
    [item] ->
      let id, kind =
        match item with
          Sig_value (id, _, _) -> id, Pvalue
        | Sig_type (id, _, _, _) -> id, Ptype
        | Sig_typext (id, _, _, _) -> id, Pconstructor
        | Sig_module (id, _, _, _, _) -> id, Pmodule
        | Sig_modtype (id, _, _) -> id, Pmodtype
        | Sig_class (id, _, _, _) -> id, Pclass
        | Sig_class_type (id, _, _, _) -> id, Pcltype
      in
      let prefix = List.tl (list_of_path path) and name = Ident.name id in
      let pos =
        try
          let chan = open_in file in
          if Filename.check_suffix file ".ml" then
            let parsed = Parse.implementation (Lexing.from_channel chan) in
            close_in chan;
            Searchid.search_structure parsed ~name ~kind ~prefix
          else
            let parsed = Parse.interface (Lexing.from_channel chan) in
            close_in chan;
            Searchid.search_signature parsed ~name ~kind ~prefix
        with _ -> 0
      in !editor_ref ~file ~pos ()
  | _ -> !editor_ref ~file ()

(* List of windows to destroy by Close All *)
let top_widgets = ref []

let dummy_item =
  Sig_modtype (Ident.create_local "dummy",
               {mtd_type=None; mtd_attributes=[]; mtd_loc=Location.none;
                mtd_uid=Uid.internal_not_actually_unique},
               Exported)

let remove_prefix ~prefix s =
  let len1 = String.length prefix and len2 = String.length s in
  if len1 > len2 then None else
  if String.sub s ~pos:0 ~len:len1 <> prefix then None else
  Some (String.sub s ~pos:len1 ~len:(len2-len1))

let rec view_signature ?title ?path ?(env = !start_env) ?(detach=false) sign =
  let env =
    match path with None -> env
    | Some path ->
        match Env.open_signature Fresh path env with
          Ok env -> env
        | Error _ -> env
  in
  let title =
    match title, path with Some title, _ -> title
    | None, Some path -> string_of_path path
    | None, None -> "Signature"
  in
  let tl, tw, finish =
    try match path, !default_frame with
      None, Some ({mw_title=Some label} as mw) when not detach ->
        Button.configure mw.mw_detach
          ~command:(fun () -> view_signature sign ~title ~env ~detach:true);
        pack [mw.mw_detach] ~side:`Left;
        Pack.forget [mw.mw_edit; mw.mw_intf];
        List.iter ~f:destroy (Winfo.children mw.mw_frame);
        Label.configure label ~text:title;
        pack [label] ~fill:`X ~side:`Bottom;
        Jg_message.formatted ~title ~on:mw.mw_frame ~maxheight:15 ()
    | None, _ -> raise Not_found
    | Some path, _ ->
        let mw =
          try find_shown_module path
          with Not_found ->
            view_module path ~env;
            find_shown_module path
        in
        (try !set_path path ~sign with _ -> ());
        begin match mw.mw_title with None -> ()
        | Some label ->
            Label.configure label ~text:title;
            pack [label] ~fill:`X ~side:`Bottom
        end;
        Button.configure mw.mw_detach
          ~command:(fun () -> view_signature sign ~title ~env ~detach:true);
        pack [mw.mw_detach] ~side:`Left;
        let repack = ref false in
        List.iter2 [mw.mw_edit; mw.mw_intf] [".ml"; ".mli"] ~f:
          begin fun button ext ->
            try
              let path = Env.normalize_module_path None env path in
              let id = head_id path in
              let name = Ident.name id in
              let name =
                match remove_prefix ~prefix:"Stdlib__" name with
                | None -> name
                | Some suff -> suff
              in
              let file = Load_path.find_normalized (name ^ ext) in
              Button.configure button
                ~command:(fun () -> edit_source ~file ~path ~sign);
              if !repack then Pack.forget [button] else
              if not (Winfo.viewable button) then repack := true;
              pack [button] ~side:`Left
            with Not_found ->
              Pack.forget [button]
          end;
        let top = Winfo.toplevel mw.mw_frame in
        if not (Winfo.ismapped top) then Wm.deiconify top;
        List.iter ~f:destroy (Winfo.children mw.mw_frame);
        Jg_message.formatted ~title ~on:mw.mw_frame ~maxheight:15 ()
    with Not_found ->
      let tl, tw, finish = Jg_message.formatted ~title ~maxheight:15 () in
      top_widgets := tl :: !top_widgets;
      tl, tw, finish
  in
  Format.set_max_boxes 100;
  Printtyp.wrap_printing_env ~error:false env
    (fun () -> Printtyp.signature Format.std_formatter sign);
  finish ();
  Lexical.init_tags tw;
  Lexical.tag tw;
  Text.configure tw ~state:`Disabled;
  let text = Jg_text.get_all tw in
  let pt =
      try Parse.interface (Lexing.from_string text)
      with Syntaxerr.Error e ->
        let l = Syntaxerr.location_of_error e in
        Jg_text.tag_and_see  tw ~start:(tpos l.loc_start.Lexing.pos_cnum)
          ~stop:(tpos l.loc_end.Lexing.pos_cnum) ~tag:"error"; []
      | Lexer.Error (_, l) ->
         let s = l.loc_start.Lexing.pos_cnum in
         let e = l.loc_end.Lexing.pos_cnum in
         Jg_text.tag_and_see tw ~start:(tpos s) ~stop:(tpos e) ~tag:"error"; []
  in
  Jg_bind.enter_focus tw;
  bind tw ~events:[`Modified([`Control], `KeyPressDetail"s")]
    ~action:(fun _ -> Jg_text.search_string tw);
  bind tw ~events:[`Modified([`Double], `ButtonPressDetail 1)]
    ~fields:[`MouseX;`MouseY] ~breakable:true
    ~action:(fun ev ->
      let `Linechar (l, c) =
        Text.index tw ~index:(`Atxy(ev.ev_MouseX,ev.ev_MouseY), []) in
      try
        match search_pos_signature pt ~pos:(lines_to_chars l ~text + c) ~env
        with [] -> break ()
        | ((kind, lid), env, loc) :: _ -> view_decl lid ~kind ~env
      with Not_found | Env.Error _ | Persistent_env.Error _ -> ());
  bind tw ~events:[`ButtonPressDetail 3] ~breakable:true
    ~fields:[`MouseX;`MouseY]
    ~action:(fun ev ->
      let x = ev.ev_MouseX and y = ev.ev_MouseY in
      let `Linechar (l, c) =
        Text.index tw ~index:(`Atxy(x,y), []) in
      try
        match search_pos_signature pt ~pos:(lines_to_chars l ~text + c) ~env
        with [] -> break ()
        | ((kind, lid), env, loc) :: _ ->
            let menu = view_decl_menu lid ~kind ~env ~parent:tw in
            let x = x + Winfo.rootx tw and y = y + Winfo.rooty tw - 10 in
            Menu.popup menu ~x ~y
      with Not_found -> ())

and view_signature_item sign ~path ~env =
  view_signature sign ~title:(string_of_path path)
    ?path:(parent_path path) ~env

and view_module path ~env =
  let modtype = find_module path env in
  match scrape_alias env modtype.md_type with
    Mty_signature sign ->
      !view_defined_ref (Searchid.longident_of_path path) ~env
  | _ ->
      let id = ident_of_path path ~default:"M" in
      view_signature_item [Sig_module (id, Mp_present, modtype,
                                       Trec_not, Exported)] ~path ~env

and view_module_id id ~env =
  let path, _ = find_module_by_name id env in
  view_module path ~env

and view_type_decl path ~env =
  let td = find_type path env in
  try match td.type_manifest with None -> raise Not_found
    | Some ty -> match get_desc ty with
        Tobject _ ->
          let clt = find_cltype path env in
          view_signature_item ~path ~env
            [Sig_class_type(ident_of_path path ~default:"ct", clt, Trec_first,
                            Exported);
             dummy_item; dummy_item]
      | Tvariant row when row_name row <> None ->
          let Row {fields; more; closed; fixed} = row_repr row in
          let row = create_row ~fields ~more ~closed ~fixed ~name:None in
          let td =
            {td with type_manifest = Some(Btype.newgenty (Tvariant row))} in
          view_signature_item ~path ~env
            [Sig_type(ident_of_path path ~default:"t", td, Trec_first,
                      Exported)]
      | _ -> raise Not_found
  with Not_found ->
    view_signature_item ~path ~env
      [Sig_type(ident_of_path path ~default:"t", td, Trec_first, Exported)]

and view_type_id li ~env =
  let path, _ = find_type_by_name li env in
  view_type_decl path ~env

and view_class_id li ~env =
  let path, cl = find_class_by_name li env in
  view_signature_item ~path ~env
     [Sig_class(ident_of_path path ~default:"c", cl, Trec_first, Exported);
      dummy_item; dummy_item; dummy_item]

and view_cltype_id li ~env =
  let path, clt = find_cltype_by_name li env in
  view_signature_item ~path ~env
     [Sig_class_type(ident_of_path path ~default:"ct", clt, Trec_first,
                     Exported);
      dummy_item; dummy_item]

and view_modtype_id li ~env =
  let path, td = find_modtype_by_name li env in
  view_signature_item ~path ~env
    [Sig_modtype(ident_of_path path ~default:"S", td, Exported)]

and view_expr_type ?title ?path ?env ?(name="noname") t =
  let title =
    match title, path with Some title, _ -> title
    | None, Some path -> string_of_path path
    | None, None -> "Expression type"
  and path, id =
    match path with None -> None, Ident.create_local name
    | Some path -> parent_path path, ident_of_path path ~default:name
  in
  view_signature ~title ?path ?env
    [Sig_value (id, {val_type = t; val_kind = Val_reg; val_attributes=[];
                     val_loc = Location.none;
                     val_uid = Uid.internal_not_actually_unique},
                Exported)]

and view_decl lid ~kind ~env =
  match kind with
    `Type -> view_type_id lid ~env
  | `Class -> view_class_id lid ~env
  | `Module -> view_module_id lid ~env
  | `Modtype -> view_modtype_id lid ~env

and view_decl_menu lid ~kind ~env ~parent =
  let path, kname =
    try match kind with
      `Type -> fst (find_type_by_name lid env), "Type"
    | `Class -> fst (find_class_by_name lid env), "Class"
    | `Module -> fst (find_module_by_name lid env), "Module"
    | `Modtype -> fst (find_modtype_by_name lid env), "Module type"
    with Env.Error _ | Persistent_env.Error _ -> raise Not_found
  in
  let menu = Menu.create parent ~tearoff:false in
  let label = kname ^ " " ^ string_of_path path in
  begin match path with
    Pident _ ->
      Menu.add_command menu ~label ~state:`Disabled
  | _ ->
      Menu.add_command menu ~label
        ~command:(fun () -> view_decl lid ~kind ~env);
  end;
  if kind = `Type || kind = `Modtype then begin
    let buf = new buffer ~size:60 in
    let (fo,ff) = Format.get_formatter_output_functions ()
    and margin = Format.get_margin () in
    Format.set_formatter_output_functions buf#out (fun () -> ());
    Format.set_margin 60;
    Format.open_hbox ();
    Printtyp.wrap_printing_env ~error:false env begin fun () ->
      if kind = `Type then
        Printtyp.type_declaration
          (ident_of_path path ~default:"t")
          Format.std_formatter
          (find_type path env)
      else
        Printtyp.modtype_declaration
          (ident_of_path path ~default:"S")
          Format.std_formatter
          (find_modtype path env)
    end;
    Format.close_box (); Format.print_flush ();
    Format.set_formatter_output_functions fo ff;
    Format.set_margin margin;
    let l = Str.split ~!"\n" buf#get in
    let font =
      let font =
        Option.get Widget.default_toplevel ~name:"font" ~clas:"Font" in
      if font = "" then "7x14" else font
    in
    (* Menu.add_separator menu; *)
    List.iter l
      ~f:(fun label -> Menu.add_command menu ~label ~font ~state:`Disabled)
  end;
  menu

(* search and view in a structure *)

type fkind = [
    `Exp of
      [`Expr|`Pat|`Const|`Val of Path.t|`Var of Path.t|`New of Path.t]
        * type_expr
  | `Class of Path.t * class_type
  | `Module of Path.t * module_type
]

let view_type kind ~env =
  match kind with
    `Exp (k, ty) ->
      begin match k with
        `Expr -> view_expr_type ty ~title:"Expression type" ~env
      | `Pat -> view_expr_type ty ~title:"Pattern type" ~env
      | `Const -> view_expr_type ty ~title:"Constant type" ~env
      | `Val path ->
          begin try
            let vd = find_value path env in
            view_signature_item ~path ~env
              [Sig_value(ident_of_path path ~default:"v", vd, Exported)]
          with Not_found ->
            view_expr_type ty ~path ~env
          end
      | `Var path ->
          let vd = find_value path env in
          view_expr_type vd.val_type ~env ~path ~title:"Variable type"
      | `New path ->
          let cl = find_class path env in
          view_signature_item ~path ~env
            [Sig_class(ident_of_path path ~default:"c", cl, Trec_first,
                       Exported)]
      end
  | `Class (path, cty) ->
      let cld = { cty_params = []; cty_variance = []; cty_type = cty;
                  cty_path = path; cty_new = None; cty_loc = Location.none;
                  cty_attributes = [];
                  cty_uid = Uid.internal_not_actually_unique } in
      view_signature_item ~path ~env
        [Sig_class(ident_of_path path ~default:"c", cld, Trec_first,
                   Exported)]
  | `Module (path, mty) ->
      match mty with
        Mty_signature sign -> view_signature sign ~path ~env
      | modtype ->
          let md =
	    {md_type = mty; md_attributes = []; md_loc = Location.none;
             md_uid = Uid.internal_not_actually_unique} in
          view_signature_item ~path ~env
            [Sig_module(ident_of_path path ~default:"M", Mp_present,
                        md, Trec_not, Exported)]

let view_type_menu kind ~env ~parent =
  let title =
    match kind with
      `Exp (`Expr,_) -> "Expression :"
    | `Exp (`Pat, _) -> "Pattern :"
    | `Exp (`Const, _) -> "Constant :"
    | `Exp (`Val path, _) -> "Value " ^ string_of_path path ^ " :"
    | `Exp (`Var path, _) ->
        "Variable " ^ Ident.name (ident_of_path path ~default:"noname") ^ " :"
    | `Exp (`New path, _) -> "Class " ^ string_of_path path ^ " :"
    | `Class (path, _) -> "Class " ^ string_of_path path ^ " :"
    | `Module (path,_) -> "Module " ^ string_of_path path in
  let menu = Menu.create parent ~tearoff:false in
  begin match kind with
    `Exp((`Expr | `Pat | `Const | `Val (Pident _)),_)  ->
      Menu.add_command menu ~label:title ~state:`Disabled
  | `Exp _ | `Class _ | `Module _ ->
      Menu.add_command menu ~label:title
        ~command:(fun () -> view_type kind ~env)
  end;
  begin match kind with `Module _ | `Class _ -> ()
  | `Exp(_, ty) ->
      let buf = new buffer ~size:60 in
      let (fo,ff) = Format.get_formatter_output_functions ()
      and margin = Format.get_margin () in
      Format.set_formatter_output_functions buf#out ignore;
      Format.set_margin 60;
      Format.open_hbox ();
      Printtyp.prepare_for_printing [ty];
      Printtyp.wrap_printing_env ~error:false env
        (fun () -> Printtyp.type_expr Format.std_formatter ty);
      Format.close_box (); Format.print_flush ();
      Format.set_formatter_output_functions fo ff;
      Format.set_margin margin;
      let l = Str.split ~!"\n" buf#get in
      let font =
        let font =
          Option.get Widget.default_toplevel ~name:"font" ~clas:"Font" in
        if font = "" then "7x14" else font
      in
      (* Menu.add_separator menu; *)
      List.iter l ~f:
        begin fun label -> match get_desc ty with
          Tconstr (path,_,_) ->
            Menu.add_command menu ~label ~font
              ~command:(fun () -> view_type_decl path ~env)
        | Tvariant row when row_name row <> None ->
            let path, _ = Stdlib.Option.get (row_name row) in
            Menu.add_command menu ~label ~font
              ~command:(fun () -> view_type_decl path ~env)
        | _ ->
            Menu.add_command menu ~label ~font ~state:`Disabled
        end
  end;
  menu

let found_str = ref ([] : (fkind * Env.t * Location.t) list)
let add_found_str = add_found ~found:found_str

let rec search_pos_structure ~pos str =
  List.iter str ~f:
  begin function str -> match str.str_desc with
    Tstr_eval (exp, _) -> search_pos_expr exp ~pos
  | Tstr_value (rec_flag, l) ->
      List.iter l ~f:
      begin fun {vb_pat=pat;vb_expr=exp} ->
        let env =
          if rec_flag = Asttypes.Recursive then exp.exp_env else Env.empty in
        search_pos_pat pat ~pos ~env;
        search_pos_expr exp ~pos
      end
  | Tstr_module mb -> search_pos_module_expr mb.mb_expr ~pos
  | Tstr_recmodule bindings ->
      List.iter bindings ~f:(fun mb -> search_pos_module_expr mb.mb_expr ~pos)
  | Tstr_class l ->
      List.iter l ~f:(fun (ci, _) -> search_pos_class_expr ci.ci_expr ~pos)
  | Tstr_include {incl_mod=m} -> search_pos_module_expr m ~pos
  | Tstr_primitive _
  | Tstr_type _
  | Tstr_typext _
  | Tstr_exception _
  | Tstr_modtype _
  | Tstr_open _
  | Tstr_class_type _
  | Tstr_attribute _
    -> ()
  end

and search_pos_class_structure ~pos cls =
  List.iter cls.cstr_fields ~f:
    begin function cf -> match cf.cf_desc with
        Tcf_inherit (_, cl, _, _, _) ->
          search_pos_class_expr cl ~pos
      | Tcf_val (_, _, _, Tcfk_concrete (_, exp), _) -> search_pos_expr exp ~pos
      | Tcf_val _ -> ()
      | Tcf_method (_, _, Tcfk_concrete (_, exp)) -> search_pos_expr exp ~pos
      | Tcf_initializer exp -> search_pos_expr exp ~pos
      | Tcf_constraint _
      | Tcf_attribute _
      | Tcf_method _
        -> () (* TODO !!!!!!!!!!!!!!!!! *)
    end

and search_pos_class_expr ~pos cl =
  if in_loc cl.cl_loc ~pos then begin
    begin match cl.cl_desc with
      Tcl_ident (path, _, _) ->
        add_found_str (`Class (path, cl.cl_type))
          ~env:!start_env ~loc:cl.cl_loc
    | Tcl_structure cls ->
        search_pos_class_structure ~pos cls
    | Tcl_fun (_, pat, iel, cl, _) ->
        search_pos_pat pat ~pos ~env:pat.pat_env;
        List.iter iel ~f:(fun (_, exp) -> search_pos_expr exp ~pos);
        search_pos_class_expr cl ~pos
    | Tcl_apply (cl, el) ->
        search_pos_class_expr cl ~pos;
        List.iter el
          ~f:(fun (_, x) -> Stdlib.Option.iter (search_pos_expr ~pos) x)
    | Tcl_let (_, pel, iel, cl) ->
        List.iter pel ~f:
          begin fun {vb_pat=pat; vb_expr=exp} ->
            search_pos_pat pat ~pos ~env:exp.exp_env;
            search_pos_expr exp ~pos
          end;
        List.iter iel ~f:(fun (_, exp) -> search_pos_expr exp ~pos);
        search_pos_class_expr cl ~pos
    | Tcl_open (_, cl)
    | Tcl_constraint (cl, _, _, _, _) ->
        search_pos_class_expr cl ~pos
    end;
    add_found_str (`Class (Pident (Ident.create_local "c"), cl.cl_type))
      ~env:!start_env ~loc:cl.cl_loc
  end

and search_case : 'a. pos:_ -> 'a case -> unit =
  fun ~pos {c_lhs; c_guard; c_rhs} ->
  search_pos_pat c_lhs ~pos ~env:c_rhs.exp_env;
  begin match c_guard with
  | None -> ()
  | Some g -> search_pos_expr g ~pos
  end;
  search_pos_expr c_rhs ~pos

and search_pos_fun_param ~pos ~env fp =
  if in_loc fp.fp_loc ~pos then begin
    match fp.fp_kind with
    | Tparam_pat pat -> search_pos_pat pat ~pos ~env
    | Tparam_optional_default (pat, exp) ->
        search_pos_pat pat ~pos ~env;
        search_pos_expr ~pos exp
  end

and search_pos_expr ~pos exp =
  if in_loc exp.exp_loc ~pos then begin
  begin match exp.exp_desc with
    Texp_ident (path, _, _) ->
      add_found_str (`Exp(`Val path, exp.exp_type))
        ~env:exp.exp_env ~loc:exp.exp_loc
  | Texp_constant v ->
      add_found_str (`Exp(`Const, exp.exp_type))
        ~env:exp.exp_env ~loc:exp.exp_loc
  | Texp_let (_, expl, exp) ->
      List.iter expl ~f:
      begin fun {vb_pat=pat; vb_expr=exp'} ->
        search_pos_pat pat ~pos ~env:exp.exp_env;
        search_pos_expr exp' ~pos
      end;
      search_pos_expr exp ~pos
  | Texp_function (fpl, fb) ->
      let env =
        match fb with
        | Tfunction_body e -> e.exp_env
        | Tfunction_cases {cases = c :: _} -> c.c_rhs.exp_env
        | Tfunction_cases {cases = []} -> exp.exp_env
      in
      List.iter ~f:(search_pos_fun_param ~pos ~env) fpl;
      begin match fb with
      | Tfunction_body e -> search_pos_expr e ~pos
      | Tfunction_cases {cases=l} -> List.iter l ~f:(search_case ~pos)
      end
  | Texp_apply (exp, l) ->
      List.iter l
        ~f:(fun (_, x) -> Stdlib.Option.iter (search_pos_expr ~pos) x);
      search_pos_expr exp ~pos
  | Texp_match (exp, l, _) ->
      search_pos_expr exp ~pos;
      List.iter l ~f:(search_case ~pos)
  | Texp_try (exp, l) ->
      search_pos_expr exp ~pos;
      List.iter l ~f:(search_case ~pos)
  | Texp_tuple l -> List.iter l ~f:(search_pos_expr ~pos)
  | Texp_construct (_, _, l) -> List.iter l ~f:(search_pos_expr ~pos)
  | Texp_variant (_, None) -> ()
  | Texp_variant (_, Some exp) -> search_pos_expr exp ~pos
  | Texp_record {fields=l; extended_expression=opt} ->
      Array.iter l ~f:
        (function (_,Overridden(_,exp)) -> search_pos_expr exp ~pos | _ -> ());
      (match opt with None -> () | Some exp -> search_pos_expr exp ~pos)
  | Texp_field (exp, _, _) -> search_pos_expr exp ~pos
  | Texp_setfield (a, _, _, b) ->
      search_pos_expr a ~pos; search_pos_expr b ~pos
  | Texp_array l -> List.iter l ~f:(search_pos_expr ~pos)
  | Texp_ifthenelse (a, b, c) ->
      search_pos_expr a ~pos; search_pos_expr b ~pos;
      begin match c with None -> ()
      | Some exp -> search_pos_expr exp ~pos
      end
  | Texp_sequence (a,b) ->
      search_pos_expr a ~pos; search_pos_expr b ~pos
  | Texp_while (a,b) ->
      search_pos_expr a ~pos; search_pos_expr b ~pos
  | Texp_for (_, _, a, b, _, c) ->
      List.iter [a;b;c] ~f:(search_pos_expr ~pos)
  | Texp_send (exp, _) -> search_pos_expr exp ~pos
  | Texp_new (path, _, _) ->
      add_found_str (`Exp(`New path, exp.exp_type))
        ~env:exp.exp_env ~loc:exp.exp_loc
  | Texp_instvar (_, path, _) ->
      add_found_str (`Exp(`Var path, exp.exp_type))
        ~env:exp.exp_env ~loc:exp.exp_loc
  | Texp_setinstvar (_, path, _, exp) ->
      search_pos_expr exp ~pos;
      add_found_str (`Exp(`Var path, exp.exp_type))
        ~env:exp.exp_env ~loc:exp.exp_loc
  | Texp_override (_, l) ->
      List.iter l ~f:(fun (_, _, exp) -> search_pos_expr exp ~pos)
  | Texp_letmodule (id, _, _, modexp, exp) ->
      search_pos_module_expr modexp ~pos;
      search_pos_expr exp ~pos
  | Texp_assert (exp, _) ->
      search_pos_expr exp ~pos
  | Texp_lazy exp ->
      search_pos_expr exp ~pos
  | Texp_object (cls, _) ->
      search_pos_class_structure ~pos cls
  | Texp_pack modexp ->
      search_pos_module_expr modexp ~pos
  | Texp_unreachable ->
      ()
  | Texp_extension_constructor _ ->
      ()
  | Texp_letexception (_, exp) ->
      search_pos_expr exp ~pos
  | Texp_letop _ ->
      ()
  | Texp_open (_, exp) ->
      search_pos_expr exp ~pos
  end;
  add_found_str (`Exp(`Expr, exp.exp_type)) ~env:exp.exp_env ~loc:exp.exp_loc
  end

and search_pos_pat : type a. pos:_ -> env:_ -> a general_pattern -> unit =
  fun ~pos ~env pat ->
  if in_loc pat.pat_loc ~pos then begin
  begin match pat.pat_desc with
    Tpat_any -> ()
  | Tpat_var (id, _, _) ->
      add_found_str (`Exp(`Val (Pident id), pat.pat_type))
        ~env ~loc:pat.pat_loc
  | Tpat_alias (pat, _, _, _)
  | Tpat_lazy pat
  | Tpat_exception pat -> search_pos_pat pat ~pos ~env
  | Tpat_value pat -> search_pos_pat (pat :> pattern) ~pos ~env
  | Tpat_constant _ ->
      add_found_str (`Exp(`Const, pat.pat_type)) ~env ~loc:pat.pat_loc
  | Tpat_tuple l ->
      List.iter l ~f:(search_pos_pat ~pos ~env)
  | Tpat_construct (_, _, l, _) ->
      List.iter l ~f:(search_pos_pat ~pos ~env)
  | Tpat_variant (_, None, _) -> ()
  | Tpat_variant (_, Some pat, _) -> search_pos_pat pat ~pos ~env
  | Tpat_record (l, _) ->
      List.iter l ~f:(fun (_, _, pat) -> search_pos_pat pat ~pos ~env)
  | Tpat_array l ->
      List.iter l ~f:(search_pos_pat ~pos ~env)
  | Tpat_or (a, b, None) ->
      search_pos_pat a ~pos ~env; search_pos_pat b ~pos ~env
  | Tpat_or (_, _, Some _) ->
      ()
  end;
  add_found_str (`Exp(`Pat, pat.pat_type)) ~env ~loc:pat.pat_loc
  end

and search_pos_module_expr ~pos (m :module_expr) =
  if in_loc m.mod_loc ~pos then begin
    begin match m.mod_desc with
      Tmod_ident (path, _) ->
        add_found_str (`Module (path, m.mod_type))
          ~env:m.mod_env ~loc:m.mod_loc
    | Tmod_structure str -> search_pos_structure str.str_items ~pos
    | Tmod_functor (_, m) -> search_pos_module_expr m ~pos
    | Tmod_apply (a, b, _) ->
        search_pos_module_expr a ~pos; search_pos_module_expr b ~pos
    | Tmod_constraint (m, _, _, _) -> search_pos_module_expr m ~pos
    | Tmod_unpack (e, _) -> search_pos_expr e ~pos
    | Tmod_apply_unit m -> search_pos_module_expr m ~pos
    end;
    add_found_str (`Module (Pident (Ident.create_local "M"), m.mod_type))
      ~env:m.mod_env ~loc:m.mod_loc
  end

let search_pos_structure ~pos str =
  observe ~ref:found_str (search_pos_structure ~pos) str

open Stypes

let search_pos_ti ~pos = function
    Ti_pat (_, p)   -> search_pos_pat ~pos ~env:p.pat_env p
  | Ti_expr e  -> search_pos_expr ~pos e
  | Ti_class c -> search_pos_class_expr ~pos c
  | Ti_mod m   -> search_pos_module_expr ~pos m
  | _ -> ()
(*
  | Partial_structure st -> search_pos_structure ~pos st
  | Partial_structure_item it -> search_pos_structure ~pos [it]
  | Partial_expression e -> search_pos_expr ~pos e
  | Partial_pattern (k, p) -> search_pos_pat ~pos ~env:p.pat_env p
  | Partial_class_expr c -> search_pos_class_expr ~pos c
  | Partial_signature sg -> search_pos_signature ~pos sg
  | Partial_signature_item si -> search_pos_signature ~pos [si]
  | Partial_module_type mt -> ()
*)

let rec search_pos_info ~pos = function
    [] -> []
  | ti :: l ->
      if in_loc ~pos (get_location ti)
      then observe ~ref:found_str (search_pos_ti ~pos) ti
      else  search_pos_info ~pos l
