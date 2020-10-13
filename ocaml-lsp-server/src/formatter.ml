open Import

let read_to_end (in_chan : in_channel) : string =
  let buf = Buffer.create 0 in
  let chunk_size = 1024 in
  let chunk = Bytes.create chunk_size in
  let rec go pos =
    let actual_len = input in_chan chunk pos chunk_size in
    if actual_len > 0 then (
      Buffer.add_subbytes buf chunk 0 actual_len;
      go pos
    )
  in
  go 0;
  Buffer.contents buf

type command_result =
  { stdout : string
  ; stderr : string
  ; status : Unix.process_status
  }

let run_command command stdin_value args : command_result =
  let command =
    match args with
    | [] -> command
    | _ -> Printf.sprintf "%s %s" command (String.concat ~sep:" " args)
  in
  let env = Unix.environment () in
  (* We cannot use Unix.open_process_args_full while we still support 4.06 *)
  let in_chan, out_chan, err_chan = Unix.open_process_full command env in
  output_string out_chan stdin_value;
  flush out_chan;
  close_out out_chan;
  let stdout = read_to_end in_chan in
  let stderr = read_to_end err_chan in
  let status = Unix.close_process_full (in_chan, out_chan, err_chan) in
  { stdout; stderr; status }

type error =
  | Unsupported_syntax of Document.Syntax.t
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }

let string_of_error = function
  | Unsupported_syntax syntax ->
    sprintf "formatting %s files is not supported"
      (Document.Syntax.human_name syntax)
  | Missing_binary { binary } ->
    sprintf
      "Unable to find %s binary. You need to install %s manually to use the \
       formatting feature."
      binary binary
  | Unexpected_result { message } -> message

type formatter =
  | Reason of Document.Kind.t
  | Ocaml of Uri.t

let args = function
  | Ocaml uri -> [ sprintf "--name=%s" (Uri.to_path uri); "-" ]
  | Reason kind -> (
    [ "--parse"; "re"; "--print"; "re" ]
    @
    match kind with
    | Impl -> []
    | Intf -> [ "--interface=true" ] )

let binary_name t =
  match t with
  | Ocaml _ -> "ocamlformat"
  | Reason _ -> "refmt"

let binary t =
  let name = binary_name t in
  match Bin.which name with
  | None -> Result.Error (Missing_binary { binary = name })
  | Some b -> Ok b

let formatter doc =
  match Document.syntax doc with
  | (Ocamllex | Menhir) as s -> Error (Unsupported_syntax s)
  | Ocaml -> Ok (Ocaml (Document.uri doc))
  | Reason -> Ok (Reason (Document.kind doc))

let exec bin args stdin =
  let refmt = Fpath.to_string bin in
  let res = run_command refmt stdin args in
  match res.status with
  | Unix.WEXITED 0 -> Result.Ok res.stdout
  | _ -> Result.Error (Unexpected_result { message = res.stderr })

let run doc =
  let open Result.O in
  let* formatter = formatter doc in
  let args = args formatter in
  let* binary = binary formatter in
  let contents = Document.source doc |> Msource.text in
  exec binary args contents

let jsonrpc_error e =
  let message = string_of_error e in
  Jsonrpc.Response.Error.make ~code:InvalidRequest ~message ()

let ocamlformat_failure (err : string (* FIXME *)) =
  Server_notification.Generic_notification
    { id = ()
    ; method_ = "ocamlformat/update-failure"
    ; params = Some (`String err)
    }

let ocamlformat_success =
  Server_notification.Generic_notification
    { id = (); method_ = "ocamlformat/update-success"; params = None }

let run rpc doc =
  let response, notif =
    match run doc with
    | Error e ->
      let err_msg = string_of_error e in
      let ocamlformat_failure = ocamlformat_failure err_msg in
      (Error (jsonrpc_error e), ocamlformat_failure)
    | Ok result ->
      let pos line col = { Position.character = col; line } in
      let range =
        let start_pos = pos 0 0 in
        match Msource.get_logical (Document.source doc) `End with
        | `Logical (l, c) ->
          let end_pos = pos l c in
          { Range.start = start_pos; end_ = end_pos }
      in
      let change = { TextEdit.newText = result; range } in
      let state = Server.state rpc in
      (Ok (Some [ change ], state), ocamlformat_success)
  in
  (* As we can't work with direct server response in vscode-languageclient, we
     have to send our own update *)
  let (_ : unit Fiber.t) =
    let state : State.t = Server.state rpc in
    Scheduler.detach state.scheduler (fun () ->
        log ~title:Logger.Title.Debug "detched send notif";
        Server.notification rpc notif)
  in
  log ~title:Logger.Title.Debug "sent response";
  response
