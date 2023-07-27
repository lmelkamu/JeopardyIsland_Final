open! Core

type t =
  { name : string
  ; position : int * int
  ; question : string
  ; mutable color : int * int * int
  }
