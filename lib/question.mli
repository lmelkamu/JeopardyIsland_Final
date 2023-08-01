open! Core
open! Async
open! Cohttp_async
open! Jsonaf

val question_command : Command.t

module Question : sig
  type t =
    { question : string
    ; answers : string list
    ; correct_answer : char
    }
end

type t = Question.t list

val get_questions : int -> t Deferred.t
val is_correct : Question.t -> char -> bool
