open! Core

(* module Visited = struct type t = | Visited | Unvisited end *)

module T = struct
  type t =
    { name : string
    ; position : int * int
    ; question : Question.Question.t
    }
  [@@deriving sexp, compare, hash, equal]
end

let create ~(name : string) ~(position : int * int) ~(question : Question.t)
  : T.t
  =
  { name; position; question }
;;

include T
include Hashable.Make (T)
