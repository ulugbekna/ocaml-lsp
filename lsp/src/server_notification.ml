open! Import
open Types

type t =
  | PublishDiagnostics of PublishDiagnosticsParams.t
  | ShowMessage of ShowMessageParams.t
  | LogMessage of ShowMessageParams.t
  | TelemetryNotification of Json.t
  | Generic_notification of Jsonrpc.Message.notification

let method_ = function
  | ShowMessage _ -> "window/showMessage"
  | PublishDiagnostics _ -> "textDocument/publishDiagnostics"
  | LogMessage _ -> "window/logMessage"
  | TelemetryNotification _ -> "telemetry/event"
  | Generic_notification { method_; _ } -> method_

let yojson_of_t = function
  | LogMessage params
  | ShowMessage params ->
    ShowMessageParams.yojson_of_t params
  | PublishDiagnostics params -> PublishDiagnosticsParams.yojson_of_t params
  | TelemetryNotification params -> params
  | Generic_notification { id = (); method_; params = None } ->
    `Assoc [ ("method", `String method_) ]
  | Generic_notification { id = (); method_; params = Some params } ->
    `Assoc [ ("method", `String method_); ("params", params) ]

let to_jsonrpc t =
  let method_ = method_ t in
  let params = Some (yojson_of_t t) in
  { Jsonrpc.Message.id = (); params; method_ }

let of_jsonrpc (r : Jsonrpc.Message.notification) =
  let open Result.O in
  match r.method_ with
  | "window/showMessage" ->
    let+ params = Jsonrpc.Message.params r ShowMessageParams.t_of_yojson in
    ShowMessage params
  | "textDocument/publishDiagnostics" ->
    let+ params =
      Jsonrpc.Message.params r PublishDiagnosticsParams.t_of_yojson
    in
    PublishDiagnostics params
  | "window/logMessage" ->
    let+ params = Jsonrpc.Message.params r ShowMessageParams.t_of_yojson in
    LogMessage params
  | "telemetry/event" ->
    let+ params = Jsonrpc.Message.params r (fun x -> x) in
    TelemetryNotification params
  | _ -> Ok (Generic_notification r)
