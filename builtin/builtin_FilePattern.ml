(* File patterns *)
(* type *)
type filePattern = {
    typename : string;
    extensions : string list;
    mactypes : string list
  }
(* /type *)

##ifdef CAMLTK
let cCAMLtoTKfilePattern fp =
  let typename = TkQuote (TkToken fp.typename) in
  let extensions =
    TkQuote (TkTokenList (List.map (fun x -> TkToken x) fp.extensions)) in
  let mactypes =
    match fp.mactypes with
    | [] -> []
    | [s] -> [TkToken s]
    | _ -> [TkQuote (TkTokenList (List.map (fun x -> TkToken x) fp.mactypes))]
  in
  TkQuote (TkTokenList (typename :: extensions :: mactypes))
##else
let cCAMLtoTKfilePattern fp =
  let typename = TkQuote (TkToken fp.typename) in
  let extensions =
    TkQuote (TkTokenList (List.map ~f:(fun x -> TkToken x) fp.extensions)) in
  let mactypes =
    match fp.mactypes with
    | [] -> []
    | [s] -> [TkToken s]
    | _ ->
        [TkQuote (TkTokenList (List.map ~f:(fun x -> TkToken x) fp.mactypes))]
  in
  TkQuote (TkTokenList (typename :: extensions :: mactypes))
##endif
