open! Core

type t =
  { name : string
  ; position : int * int
  ; question : Question.t
  }

include Hashable.S with type t := t
