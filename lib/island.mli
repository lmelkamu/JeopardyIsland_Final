open! Core

type t =
  { name : string
  ; position : int * int
  ; color : int * int * int
  }

include Hashable.S with type t := t
