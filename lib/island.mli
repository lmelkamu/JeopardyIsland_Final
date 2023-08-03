open! Core

type t =
  { name : string
  ; position : int * int
  }
[@@deriving sexp]

val create : name:string -> position:int * int -> t

include Hashable.S with type t := t
include Comparable.S with type t := t
