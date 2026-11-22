type 'a t = (Syntax.Id.t * 'a) list

let empty = []

let extend x tau env = (x, tau) :: env

let lookup x env =
  env
  |> List.find_opt (fun (y, _) -> Syntax.Id.equal x y)
  |> Option.map snd
