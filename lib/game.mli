open! Core

module Level : sig
  module T : sig
    type t =
      | Easy
      | Medium
      | Hard
    [@@deriving enumerate, sexp]
  end
end

type t =
  { difficulty : Level.T.t
  ; mutable islands : Island.t list
  ; map : (Island.t, Island.t list) Hashtbl.t
  }

val game_command : Command.t
