open Type

let f tau =
  let table = Hashtbl.create 8 in
  let rec loop tau =
    match Type.repr tau with
    | TVar { contents = Unbound (id, level) } when !Type.current_level < level ->
        begin match Hashtbl.find_opt table id with
        | Some g -> g
        | None ->
            let g = TGen (Hashtbl.length table) in
            Hashtbl.add table id g;
            g
        end

    | TFun (a, b) ->
        TFun (loop a, loop b)
    
    | t -> t
  in
  loop tau