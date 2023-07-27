open! Core

type t =
  { name : string
  ; position : int * int
  ; question : Question.t
  ; color : int * int * int
  }
