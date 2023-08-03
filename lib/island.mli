open! Core

type t =
  { name : string
  ; position : int * int
  }

val create : name:string -> position:int * int -> t
val create : string -> int * int -> Question.t -> t

include Hashable.S with type t := t
