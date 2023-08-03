open! Core

type t =
  { name : string
  ; position : int * int
  }

val create : name:string -> position:int * int -> t

include Hashable.S with type t := t
