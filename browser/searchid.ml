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
open Location
open Longident
open Path
open Types
open Typedtree
open Env
open Btype
open Ctype

(* only empty here, but replaced by Pervasives later *)
let start_env = ref Env.empty
let module_list = ref []

type pkind =
    Pvalue
  | Ptype
  | Plabel
  | Pconstructor
  | Pmodule
  | Pmodtype
  | Pclass
  | Pcltype

let string_of_kind = function
    Pvalue -> "v"
  | Ptype -> "t"
  | Plabel -> "l"
  | Pconstructor -> "cn"
  | Pmodule -> "m"
  | Pmodtype -> "s"
  | Pclass -> "c"
  | Pcltype -> "ct"

let rec longident_of_path = function
    Pident id -> Lident (Ident.name id)
  | Pdot (path, s) -> Ldot (longident_of_path path, s)
  | Papply (p1, p2) -> Lapply (longident_of_path p1, longident_of_path p2)
  | Pextra_ty (p, Pext_ty) -> longident_of_path p
  | Pextra_ty (p, Pcstr_ty s) -> Ldot (longident_of_path p, s)

let rec remove_prefix lid ~prefix =
  let rec remove_hd lid ~name =
  match lid with
    Ldot (Lident s1, s2) when s1 = name -> Lident s2
  | Ldot (l, s) -> Ldot (remove_hd ~name l, s)
  | _ -> raise Not_found
  in
  match prefix with
    [] -> lid
  | name :: prefix ->
    try remove_prefix ~prefix (remove_hd ~name lid)
    with Not_found -> lid

let rec permutations l = match l with
    [] | [_] -> [l]
  | [a;b] -> [l; [b;a]]
  | _ ->
  let _, perms =
    List.fold_left l ~init:(l,[]) ~f:
    begin fun (l, perms) a ->
      let l = List.tl l in
      l @ [a],
      List.map (permutations l) ~f:(fun l -> a :: l) @ perms
    end
  in perms

let rec choose n ~card:l =
  let len = List.length l in
  if n = len then [l] else
  if n = 1 then List.map l ~f:(fun x -> [x]) else
  if n = 0 then [[]] else
  if n > len then [] else
  match l with [] -> []
  | a :: l ->
    List.map (choose (n-1) ~card:l) ~f:(fun l -> a :: l)
    @ choose n ~card:l

let rec arr p ~card:n =
  if p = 0 then 1 else n * arr (p-1) ~card:(n-1)

let rec all_args ty =
  match get_desc ty with
    Tarrow(l, ty1, ty2, _) -> let (tl,ty) = all_args ty2 in ((l,ty1)::tl, ty)
  | _ -> ([], ty)

let rec equal ~prefix t1 t2 =
  match get_desc t1, get_desc t2 with
    Tvar _, Tvar _ -> true
  | Tvariant row1, Tvariant row2 ->
      let fields1 = filter_row_fields false (row_fields row1)
      and fields2 = filter_row_fields false (row_fields row1)
      in
      let r1, r2, pairs = merge_row_fields fields1 fields2 in
      row_closed row1 = row_closed row2 && r1 = [] && r2 = [] &&
      List.for_all pairs ~f:
           begin fun (_,f1,f2) ->
             match row_field_repr f1, row_field_repr f2 with
               Rpresent None, Rpresent None -> true
             | Rpresent(Some t1), Rpresent (Some t2) -> equal t1 t2 ~prefix
             | Reither(c1, tl1, _), Reither(c2, tl2, _) ->
                 c1 = c2 && List.length tl1 = List.length tl2 &&
                 List.for_all2 tl1 tl2 ~f:(equal ~prefix)
             | _ -> false
           end
  | Tarrow _, Tarrow _ ->
      let l1, t1 = all_args t1 and l2, t2 = all_args t2 in
      equal t1 t2 ~prefix &&
      List.length l1 = List.length l2 &&
      List.exists (permutations l1) ~f:
      begin fun l1 ->
        List.for_all2 l1 l2 ~f:
        begin fun (p1,t1) (p2,t2) ->
          (p1 = Nolabel || p1 = p2) && equal t1 t2 ~prefix
        end
      end
  | Ttuple l1, Ttuple l2 ->
      List.length l1 = List.length l2 &&
      List.for_all2 l1 l2 ~f:(equal ~prefix)
  | Tconstr (p1, l1, _), Tconstr (p2, l2, _) ->
      remove_prefix ~prefix (longident_of_path p1) = (longident_of_path p2)
      && List.length l1 = List.length l2
      && List.for_all2 l1 l2 ~f:(equal ~prefix)
  | _ -> false

let get_options = List.filter ~f:Btype.is_optional

let rec included ~prefix t1 t2 =
  match get_desc t1, get_desc t2 with
    Tvar _, _ -> true
  | Tvariant row1, Tvariant row2 ->
      let fields1 = filter_row_fields false (row_fields row1)
      and fields2 = filter_row_fields false (row_fields row2)
      in
      let r1, r2, pairs = merge_row_fields fields1 fields2 in
      r1 = [] &&
      List.for_all pairs ~f:
           begin fun (_,f1,f2) ->
             match row_field_repr f1, row_field_repr f2 with
               Rpresent None, Rpresent None -> true
             | Rpresent(Some t1), Rpresent (Some t2) -> included t1 t2 ~prefix
             | Reither(c1, tl1, _), Reither(c2, tl2, _) ->
                 c1 = c2 && List.length tl1 = List.length tl2 &&
                 List.for_all2 tl1 tl2 ~f:(included ~prefix)
             | _ -> false
           end
  | Tarrow _, Tarrow _ ->
      let l1, t1 = all_args t1 and l2, t2 = all_args t2 in
      included t1 t2 ~prefix &&
      let len1 = List.length l1 and len2 = List.length l2 in
      let l2 = if arr len1 ~card:len2 < 100 then l2 else
          let ll1 = get_options (fst (List.split l1)) in
          List.filter l2
          ~f:(fun (l,_) -> not (is_optional l) || List.mem l ~set:ll1)
      in
      len1 <= len2 &&
      List.exists (List2.flat_map ~f:permutations (choose len1 ~card:l2)) ~f:
      begin fun l2 ->
        List.for_all2 l1 l2 ~f:
        begin fun (p1,t1) (p2,t2) ->
          (p1 = Nolabel || p1 = p2) && included t1 t2 ~prefix
        end
      end
  | Ttuple l1, Ttuple l2 ->
      let len1 = List.length l1 in
      len1 <= List.length l2 &&
      List.exists (List2.flat_map ~f:permutations (choose len1 ~card:l2)) ~f:
      begin fun l2 ->
        List.for_all2 l1 l2 ~f:(included ~prefix)
      end
  | _, Ttuple _ -> included (newty (Ttuple [t1])) t2 ~prefix
  | Tconstr (p1, l1, _), Tconstr (p2, l2, _) ->
      remove_prefix ~prefix (longident_of_path p1) = (longident_of_path p2)
      && List.length l1 = List.length l2
      && List.for_all2 l1 l2 ~f:(included ~prefix)
  | _ -> false

let mklid = function
    [] -> raise (Invalid_argument "Searchid.mklid")
  | x :: l ->
      List.fold_left l ~init:(Lident x) ~f:(fun acc x -> Ldot (acc, x))

let mkpath = function
    [] -> raise (Invalid_argument "Searchid.mklid")
  | x :: l ->
      List.fold_left l ~init:(Pident (Ident.create_local x))
      ~f:(fun acc x -> Pdot (acc, x))

let get_fields ~prefix ~sign self =
  (*let env = open_signature Fresh (mkpath prefix) sign !start_env in*)
  let env = add_signature sign !start_env in
  match get_desc (expand_head env self) with
    Tobject (ty_obj, _) ->
      let l,_ = flatten_fields ty_obj in l
  | _ -> []

let rec search_type_in_signature t ~sign ~prefix ~mode =
  let matches = match mode with
        `Included -> included t ~prefix
      | `Exact -> equal t ~prefix
  and lid_of_id id = mklid (prefix @ [Ident.name id]) in
  let constructor_matches = function
      Types.Cstr_tuple l -> List.exists l ~f:matches
    | Cstr_record l -> List.exists l ~f:(fun d -> matches d.Types.ld_type)
  in
  List2.flat_map sign ~f:
  begin fun item -> match item with
        Sig_value (id, vd, _) ->
          if matches vd.val_type then [lid_of_id id, Pvalue] else []
      | Sig_type (id, td, _, _) ->
          if
          matches (newconstr (Pident id) td.type_params) ||
          begin match td.type_manifest with
            None -> false
          | Some t -> matches t
          end ||
          begin match td.type_kind with
            Type_abstract _
	  | Type_open -> false
          | Type_variant (l, _) ->
            List.exists l ~f:
            begin fun {Types.cd_args=args; cd_res=r} ->
              constructor_matches args  ||
              match r with None -> false | Some x -> matches x
            end
          | Type_record(l, rep) ->
            List.exists l ~f:(fun {Types.ld_type=t} -> matches t)
          end
          then [lid_of_id id, Ptype] else []
      | Sig_typext (id, l, _, _) ->
          if constructor_matches l.ext_args
          then [lid_of_id id, Pconstructor]
          else []
      | Sig_module (id, _, {md_type=Mty_signature sign}, _, _) ->
          search_type_in_signature t ~sign ~mode
            ~prefix:(prefix @ [Ident.name id])
      | Sig_module _ -> []
      | Sig_modtype _ -> []
      | Sig_class (id, cl, _, _) ->
          let self = self_type cl.cty_type in
          if matches self
          || (match cl.cty_new with None -> false | Some ty -> matches ty)
          (* || List.exists (get_fields ~prefix ~sign self)
              ~f:(fun (_,_,ty_field) -> matches ty_field) *)
          then [lid_of_id id, Pclass] else []
      | Sig_class_type (id, cl, _, _) ->
          let self = self_type cl.clty_type in
          if matches self
          (* || List.exists (get_fields ~prefix ~sign self)
              ~f:(fun (_,_,ty_field) -> matches ty_field) *)
          then [lid_of_id id, Pclass] else []
  end

let search_all_types t ~mode =
  let tl = match mode, get_desc t with
      `Exact, _ -> [t]
    | `Included, Tarrow _ -> [t]
    | `Included, _ ->
      [t; newty(Tarrow(Nolabel,t,newvar(),commu_ok));
          newty(Tarrow(Nolabel,newvar(),t,commu_ok))]
  in List2.flat_map !module_list ~f:
    begin fun modname ->
    let mlid = Lident modname in
    try match Env.find_module_by_name mlid !start_env
    with _, {md_type=Mty_signature sign} ->
        List2.flat_map tl
          ~f:(search_type_in_signature ~sign ~prefix:[modname] ~mode)
    | _ -> []
    with Not_found | Env.Error _ | Persistent_env.Error _ -> []
    end

exception Error of int * int

let search_string_type text ~mode =
  try
    let sexp = Parse.interface (Lexing.from_string ("val z : " ^ text)) in
    let sign =
      try (Typemod.type_interface !start_env sexp).sig_type with _ ->
      let env = List.fold_left !module_list ~init:!start_env ~f:
        begin fun acc m ->
          match open_pers_signature m acc with
            Ok env -> env
          | Error _ -> acc
        end in
      try (Typemod.type_interface env sexp).sig_type
      with
        Env.Error _ | Persistent_env.Error _ -> []
      | Typemod.Error (l,_,_) ->
          let start_c = l.loc_start.Lexing.pos_cnum in
          let end_c = l.loc_end.Lexing.pos_cnum in
          raise (Error (start_c - 8, end_c - 8))
      | Typetexp.Error (l,_,_) ->
          let start_c = l.loc_start.Lexing.pos_cnum in
          let end_c = l.loc_end.Lexing.pos_cnum in
          raise (Error (start_c - 8, end_c - 8))
    in match sign with
        [ Sig_value (_, vd, _) ] ->
          search_all_types vd.val_type ~mode
      | _ -> []
  with
    Syntaxerr.Error(Syntaxerr.Unclosed(l,_,_,_)) ->
      let start_c = l.loc_start.Lexing.pos_cnum in
      let end_c = l.loc_end.Lexing.pos_cnum in
      raise (Error (start_c - 8, end_c - 8))
  | Syntaxerr.Error(Syntaxerr.Other l) ->
      let start_c = l.loc_start.Lexing.pos_cnum in
      let end_c = l.loc_end.Lexing.pos_cnum in
      raise (Error (start_c - 8, end_c - 8))
  | Lexer.Error (_, l) ->
      let start_c = l.loc_start.Lexing.pos_cnum in
      let end_c = l.loc_end.Lexing.pos_cnum in
      raise (Error (start_c - 8, end_c - 8))

let longident_of_string text =
  let exploded = ref [] and l = ref 0 in
  for i = 0 to String.length text - 2 do
    if text.[i] ='.' then
    (exploded := String.sub text ~pos:!l ~len:(i - !l) :: !exploded; l := i+1)
  done;
  let sym = String.sub text ~pos:!l ~len:(String.length text - !l) in
  let rec mklid = function
      [s] -> Lident s
    | s :: l -> Ldot (mklid l, s)
    | [] -> assert false in
  sym, fun l -> mklid (sym :: !exploded @ l)


let explode s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := s.[i] :: !l
  done; !l

let rec check_match ~pattern s =
  match pattern, s with
    [], [] -> true
  | '*'::l, l' -> check_match ~pattern:l l'
                  || check_match ~pattern:('?'::'*'::l) l'
  | '?'::l, _::l' -> check_match ~pattern:l l'
  | x::l, y::l' when x == y -> check_match ~pattern:l l'
  | _ -> false

let search_pattern_symbol text =
  if text = "" then [] else
  let pattern = explode text in
  let check i = check_match ~pattern (explode (Ident.name i)) in
  let l = List.map !module_list ~f:
    begin fun modname -> Lident modname,
    try match find_module_by_name (Lident modname) !start_env with
    | _, {md_type=Mty_signature sign} ->
        List2.flat_map sign ~f:
          begin function
            Sig_value (i, _, _) when check i -> [i, Pvalue]
          | Sig_type (i, _, _, _) when check i -> [i, Ptype]
          | Sig_typext (i, _, _, _) when check i -> [i, Pconstructor]
          | Sig_module (i, _, _, _, _) when check i -> [i, Pmodule]
          | Sig_modtype (i, _, _) when check i -> [i, Pmodtype]
          | Sig_class (i, cl, _, _) when check i
            || List.exists
                (get_fields ~prefix:[modname] ~sign (self_type cl.cty_type))
                ~f:(fun (name,_,_) -> check_match ~pattern (explode name))
            -> [i, Pclass]
          | Sig_class_type (i, cl, _, _) when check i
            || List.exists
                (get_fields ~prefix:[modname] ~sign (self_type cl.clty_type))
                ~f:(fun (name,_,_) -> check_match ~pattern (explode name))
            -> [i, Pcltype]
          | _ -> []
          end
    | _ -> []
    with Env.Error _ | Persistent_env.Error _ -> []
    end
  in
  List2.flat_map l ~f:
    begin fun (m, l) ->
      List.map l ~f:(fun (i, p) -> Ldot (m, Ident.name i), p)
    end

(*
let is_pattern s =
  try for i = 0 to String.length s -1 do
      if s.[i] = '?' || s.[i] = '*' then raise Exit
    done; false
  with Exit -> true
*)

let search_string_symbol text =
  if text = "" then [] else
  let lid = snd (longident_of_string text) [] in
  let try_lookup f k =
    try let _ = f lid !start_env in [lid, k]
    with Not_found | Env.Error _ | Persistent_env.Error _ -> []
  in
  try_lookup find_constructor_by_name Pconstructor @
  try_lookup find_module_by_name Pmodule @
  try_lookup find_modtype_by_name Pmodtype @
  try_lookup find_value_by_name Pvalue @
  try_lookup find_type_by_name Ptype @
  try_lookup find_label_by_name Plabel @
  try_lookup find_class_by_name Pclass

open Parsetree

let rec bound_variables pat =
  match pat.ppat_desc with
    Ppat_any | Ppat_constant _ | Ppat_type _ | Ppat_unpack _
  | Ppat_interval _ -> []
  | Ppat_var s -> [s.txt]
  | Ppat_alias (pat,s) -> s.txt :: bound_variables pat
  | Ppat_tuple l -> List2.flat_map l ~f:bound_variables
  | Ppat_construct (_,None) -> []
  | Ppat_construct (_,Some (_, pat)) -> bound_variables pat
  | Ppat_variant (_,None) -> []
  | Ppat_variant (_,Some pat) -> bound_variables pat
  | Ppat_record (l, _) ->
      List2.flat_map l ~f:(fun (_,pat) -> bound_variables pat)
  | Ppat_array l ->
      List2.flat_map l ~f:bound_variables
  | Ppat_or (pat1,pat2) ->
      bound_variables pat1 @ bound_variables pat2
  | Ppat_constraint (pat,_) -> bound_variables pat
  | Ppat_lazy pat -> bound_variables pat
  | Ppat_extension _ -> []
  | Ppat_effect (pat1, pat2) ->
      bound_variables pat1 @ bound_variables pat2
  | Ppat_exception pat -> bound_variables pat
  | Ppat_open (_, pat) -> bound_variables pat

let search_structure str ~name ~kind ~prefix =
  let loc = ref 0 in
  let rec search_module str ~prefix =
    match prefix with [] -> str
    | modu::prefix ->
        let str =
          List.fold_left ~init:[] str ~f:
            begin fun acc item ->
              match item.pstr_desc with
                Pstr_module x when x.pmb_name.txt = Some modu ->
                  loc := x.pmb_expr.pmod_loc.loc_start.Lexing.pos_cnum;
                  begin match x.pmb_expr.pmod_desc with
                    Pmod_structure str -> str
                  | _ -> []
                  end
              | _ -> acc
            end
        in search_module str ~prefix
  in
  List.iter (search_module str ~prefix) ~f:
    begin fun item ->
      if match item.pstr_desc with
        Pstr_value (_, l) when kind = Pvalue ->
          List.iter l ~f:
            begin fun {pvb_pat=pat} ->
              if List.mem name ~set:(bound_variables pat)
              then loc := pat.ppat_loc.loc_start.Lexing.pos_cnum
            end;
          false
      | Pstr_primitive vd when kind = Pvalue -> name = vd.pval_name.txt
      | Pstr_type (_, l) when kind = Ptype ->
          List.iter l ~f:
            begin fun td ->
              if td.ptype_name.txt = name
	      then loc := td.ptype_loc.loc_start.Lexing.pos_cnum
            end;
          false
      | Pstr_typext l when kind = Ptype ->
          List.iter l.ptyext_constructors ~f:
            begin fun td ->
              if td.pext_name.txt = name
	      then loc := td.pext_loc.loc_start.Lexing.pos_cnum
            end;
          false
      | Pstr_exception pcd when kind = Pconstructor ->
          name = pcd.ptyexn_constructor.pext_name.txt
      | Pstr_module x when kind = Pmodule -> Some name = x.pmb_name.txt
      | Pstr_modtype x when kind = Pmodtype -> name = x.pmtd_name.txt
      | Pstr_class l when kind = Pclass || kind = Ptype || kind = Pcltype ->
          List.iter l ~f:
            begin fun c ->
              if c.pci_name.txt = name
              then loc := c.pci_loc.loc_start.Lexing.pos_cnum
            end;
          false
      | Pstr_class_type l when kind = Pcltype || kind = Ptype ->
          List.iter l ~f:
            begin fun c ->
              if c.pci_name.txt = name
              then loc := c.pci_loc.loc_start.Lexing.pos_cnum
            end;
          false
      | _ -> false
      then loc := item.pstr_loc.loc_start.Lexing.pos_cnum
    end;
  !loc

let search_signature sign ~name ~kind ~prefix =
  ignore (name = "");
  ignore (prefix = [""]);
  let loc = ref 0 in
  let rec search_module_type sign ~prefix =
    match prefix with [] -> sign
    | modu::prefix ->
        let sign =
          List.fold_left ~init:[] sign ~f:
            begin fun acc item ->
              match item.psig_desc with
                Psig_module pmd when pmd.pmd_name.txt = Some modu ->
                  loc := pmd.pmd_type.pmty_loc.loc_start.Lexing.pos_cnum;
                  begin match pmd.pmd_type.pmty_desc with
                    Pmty_signature sign -> sign
                  | _ -> []
                  end
              | _ -> acc
            end
        in search_module_type sign ~prefix
  in
  List.iter (search_module_type sign ~prefix) ~f:
    begin fun item ->
      if match item.psig_desc with
        Psig_value vd when kind = Pvalue -> name = vd.pval_name.txt
      | Psig_type (_, l) when kind = Ptype ->
          List.iter l ~f:
            begin fun td ->
              if td.ptype_name.txt = name
	      then loc := td.ptype_loc.loc_start.Lexing.pos_cnum
            end;
          false
      | Psig_typext l when kind = Pconstructor ->
          List.iter l.ptyext_constructors ~f:
            begin fun td ->
              if td.pext_name.txt = name
	      then loc := td.pext_loc.loc_start.Lexing.pos_cnum
            end;
          false
      | Psig_exception pcd when kind = Pconstructor ->
          name = pcd.ptyexn_constructor.pext_name.txt
      | Psig_module pmd when kind = Pmodule -> Some name = pmd.pmd_name.txt
      | Psig_modtype pmtd when kind = Pmodtype -> name = pmtd.pmtd_name.txt
      | Psig_class l when kind = Pclass || kind = Ptype || kind = Pcltype ->
          List.iter l ~f:
            begin fun c ->
              if c.pci_name.txt = name
              then loc := c.pci_loc.loc_start.Lexing.pos_cnum
            end;
          false
      | Psig_class_type l when kind = Ptype || kind = Pcltype ->
          List.iter l ~f:
            begin fun c ->
              if c.pci_name.txt = name
              then loc := c.pci_loc.loc_start.Lexing.pos_cnum
            end;
          false
      | _ -> false
      then loc := item.psig_loc.loc_start.Lexing.pos_cnum
    end;
  !loc
