open! Core

type t =
  { name : string
  ; position : int * int
  ; question : Question.t
  }
[@@deriving equal]

val create : string -> int*int -> Question.t ->  
include Hashable.S with type t := t
