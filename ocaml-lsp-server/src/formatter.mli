open Import

(** Relies on [ocamlformat] for OCaml and [refmt] for reason *)
val run :
     'state Server.t
  -> Document.t
  -> (TextEdit.t list option * 'state, Jsonrpc.Response.Error.t) result
