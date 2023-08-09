open! Core

type t =
  { name : string
  ; position : int * int
  ; team : bool option
  ; question : Question.Question.t
  }
[@@deriving sexp]

val create
  :  name:string
  -> position:int * int
  -> question:Question.Question.t
  -> ?team:bool
  -> unit
  -> t

include Hashable.S with type t := t
include Comparable.S with type t := t
