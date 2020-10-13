open Import

(** Relies on [ocamlformat] for OCaml and [refmt] for reason *)
val run :
     State.t Server.t
  -> Document.t
  -> (TextEdit.t list option * State.t, Jsonrpc.Response.Error.t) result
