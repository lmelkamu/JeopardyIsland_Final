open! Core

module T = struct
  type t =
    { name : string
    ; position : int * int
    ; color : int * int * int
    }
  [@@deriving sexp, compare, hash]
end

include T
include Hashable.Make (T)
