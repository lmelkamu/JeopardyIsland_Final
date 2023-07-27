open! Core

type t =
  { name : string
  ; position : int * int
  ; question : Question.t
  ; mutable color : int * int * int
  }
