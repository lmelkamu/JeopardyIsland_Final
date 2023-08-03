open! Core

(* module Upgrade : sig type t = | Double_points | Two_chances | No_loss
   end *)

type t =
  { name : string
  ; mutable points : int
  ; mutable curr_island : Island.t (* ; mutable upgrades : Upgrade.t *)
  }

val create : string -> Island.t -> t
val equal : t -> t -> bool
