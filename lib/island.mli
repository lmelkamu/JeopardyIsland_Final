open! Core

type t =
  { name : string
  ; position : int * int
  ; question : Question.Question.t
  ; color : int * int * int
  }

include Hashable.S with type t := t
