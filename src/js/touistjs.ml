


let _ =
  Js.export_all (
    object%js
      method tolatex t =
        try let a,_ = Parse.parse_sat (Js.to_string t) in Js.string (Latex.latex_of_ast a)
        with Msgs.Fatal m -> Json.output (List.fold_left (
          fun acc (typ,dur,msg,loc) -> (Msgs.string_of_type typ, Msgs.string_of_during dur, msg, Msgs.get_loc loc)::acc
          ) [] (Msgs.elements m))
     end
  )