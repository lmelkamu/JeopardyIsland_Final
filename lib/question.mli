open! Core
open! Async
open! Cohttp_async
open! Jsonaf
<<<<<<< HEAD
open! Ppxlib
=======
>>>>>>> 3468c27 (setting up skeleton for react later)

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

val get_questions : int -> Questions.t Deferred.t
val is_correct : Question.t -> string -> bool
