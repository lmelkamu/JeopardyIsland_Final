open! Core

(* module Upgrade : sig type t = | Double_points | Two_chances | No_loss
   end *)

type t =
  { name : string
  ; mutable points : int
  ; mutable curr_island : Island.t (* ; mutable upgrades : Upgrade.t *)
  }
[@@deriving sexp]

val create : name:string -> island:Island.t -> t
val equal : t -> t -> bool
