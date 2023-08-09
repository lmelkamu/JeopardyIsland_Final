open! Core

(* module Visited = struct type t = | Visited | Unvisited end *)

module T = struct
  type t =
    { name : string
    ; position : int * int
    ; team : bool option
    ; question : Question.Question.t
    }
  [@@deriving sexp, compare, hash, equal]
end

include T
include Hashable.Make (T)
include Comparable.Make (T)

let create
  ~(name : string)
  ~(position : int * int)
  ~(question : Question.Question.t)
  ?team
  ()
  : t
  =
  { name; position; team; question }
;;
