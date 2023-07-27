open! Core
open! Async
open! Cohttp_async
open! Jsonaf
(* type t *)

val question_command : Command.t

module Question : sig
  type t =
    { question : string
    ; correct_answer : string
    ; incorrect_answers : string list
    }
end

module Questions : sig
  type t = { results : Question.t list }
end
