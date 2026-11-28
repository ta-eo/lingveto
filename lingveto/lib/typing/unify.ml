exception Occurs
exception Mismatch of Ty.t * Ty.t

open Ty

let rec occurs id level tau =
  match Ty.repr tau with
  
  | TVar ({ contents = Unbound (id', level') } as r) ->
      if id = id' then
        raise Occurs;
      if level < level' then
        r := Unbound (id', level)
  
  | TFun (a, b) ->
      occurs id level a;
      occurs id level b
  
  | _ -> ()


let rec f tau1 tau2 =
  let tau1 = Ty.repr tau1
  and tau2 = Ty.repr tau2 in

  if tau1 == tau2 then
    ()
  else
    let open Ty in
    match tau1, tau2 with

    | TInt, TInt -> ()

    | TBool, TBool -> ()
    
    | TFun (a1, b1), TFun (a2, b2) ->
        f a1 a2;
        f b1 b2
    
    | TVar r1, TVar r2 when r1 == r2 ->
        ()

    | TVar ({ contents = Unbound (id, level) } as r), t
    | t, TVar ({ contents = Unbound (id, level) } as r) ->
        occurs id level t;
        r := Link t
    
    | tau1, tau2 ->
        raise (Mismatch (tau1, tau2))