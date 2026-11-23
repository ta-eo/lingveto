(* bin/main.ml *)
open Lingveto

let read_file path = In_channel.with_open_bin path In_channel.input_all

let () =
  match Sys.argv with
  | [| _; path |] ->
      let src =
        try read_file path with Sys_error msg -> prerr_endline msg; exit 2
      in
      begin match Driver.eval_string ~fname:path src with
      | Ok (v, ty) ->
          Printf.printf "%s : %s\n"
            (Interpreter.Value.to_string v)
            (Typing.Ty.to_string ty)
      | Error e ->
          Format.eprintf "%a@." Driver.pp_error e;
          exit 1
      end
  | _ ->
      prerr_endline "uzo: lingveto <dosiero>";
      exit 2