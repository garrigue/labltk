##ifdef CAMLTK

let bind widget tag eventsequence action =
  tkCommand [|
    cCAMLtoTKwidget widget_canvas_table widget;
    TkToken "bind";
    cCAMLtoTKtagOrId tag;
    cCAMLtoTKeventSequence eventsequence;
    begin match action with
    | BindRemove -> TkToken ""
    | BindSet (what, f) ->
        let cbId = register_callback widget ~callback:(wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what))
    | BindSetBreakable (what, f) ->
        let cbId = register_callback widget ~callback:(wrapeventInfo f what) in
        TkToken ("camlcb " ^ cbId ^ (writeeventField what)^
                 " ; if { $BreakBindingsSequence == 1 } then { break ;} ; \
                   set BreakBindingsSequence 0")
    | BindExtend (what, f) ->
        let cbId = register_callback widget ~callback:(wrapeventInfo f what) in
        TkToken ("+camlcb " ^ cbId ^ (writeeventField what))
    end
 |]
;;

##else

let bind ~events
    ?(extend = false) ?(breakable = false) ?(fields = [])
    ?action widget tag =
  tkCommand
    [| cCAMLtoTKwidget widget;
       TkToken "bind";
       cCAMLtoTKtagOrId tag;
       cCAMLtoTKeventSequence events;
       begin match action with None -> TkToken ""
       | Some f ->
           let cbId =
             register_callback widget ~callback: (wrapeventInfo f fields) in
           let cb = if extend then "+camlcb " else "camlcb " in
           let cb = cb ^ cbId ^ writeeventField fields in
           let cb =
             if breakable then
               cb ^ " ; if { $BreakBindingsSequence == 1 } then { break ;}"
               ^ " ; set BreakBindingsSequence 0"
             else cb in
           TkToken cb
       end
     |]
;;

##endif
