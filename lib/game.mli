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

module Game_state : sig
  type t =
    | Start
    | Game_over
    | Answering of Player.t
    | Buzzing
    | Selecting of Player.t
  [@@deriving sexp_of, compare]

  val to_string : t -> string
end

type t =
  { player_one : Player.t
  ; player_two : Player.t
  ; mutable game_state : Game_state.t
  ; difficulty : Level.T.t
  ; mutable islands : Island.t list
  ; map : (Island.t, Island.t list) Hashtbl.t
  }

val game_command : Command.t
val handle_key : t -> char -> unit
