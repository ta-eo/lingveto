open Ty

let f tau =
  let table = Hashtbl.create 8 in
  let rec loop = function
    | TGen n ->
        begin match Hashtbl.find_opt table n with
        | Some v -> v
        | None ->
            let v = fresh_type_var () in
            Hashtbl.add table n v;
            v
        end
    | TFun (a, b) ->
        TFun (loop a, loop b)
    | TVar { contents = Link t } ->
        loop t
    | t -> t
  in
  loop tau