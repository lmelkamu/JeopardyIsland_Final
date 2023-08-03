open! Core

(* module Visited = struct type t = | Visited | Unvisited end *)

module T = struct
  type t =
    { name : string
    ; position : int * int
    }
  [@@deriving sexp, compare, hash, equal]
end

let create ~(name : string) ~(position : int * int) : T.t =
  { name; position }
;;

include T
include Hashable.Make (T)
include Comparable.Make (T)
