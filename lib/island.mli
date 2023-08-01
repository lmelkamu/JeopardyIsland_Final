open! Core

type t =
  { name : string
  ; position : int * int
  ; color : int * int * int
  }
[@@deriving equal]

include Hashable.S with type t := t
