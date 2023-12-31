open! Core
open! Async

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
    | Start of int * int
    | Game_over of Player.t
    | Answering of Player.t
    | Buzzing
    | Selecting of Player.t * int option
    | Correct_answer of bool
  (* [@@deriving sexp_of, compare] *)

  val to_string : t -> string
end

type t =
  { player_one : Player.t
  ; player_two : Player.t
  ; mutable curr_player : Player.t
  ; mutable game_state : Game_state.t
  ; mutable islands : Island.t list
  ; map : (Island.t, Island.Set.t) Hashtbl.t
  ; mutable selected_island : Island.t option
  ; mutable visisted_islands : Island.t list
  ; mutable current_question : Question.Question.t
  }

(* val game_command : Command.t *)
val handle_key : t -> char -> unit
val create : Level.T.t -> string -> string -> t Deferred.t
