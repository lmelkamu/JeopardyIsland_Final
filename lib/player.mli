open! Core

module Upgrade : sig
  type t =
    | Double_points
    | Two_chances
    | No_loss
end

type t =
  { name : string
  ; mutable points : int
  ; curr_island : Island.t
  ; mutable upgrades : Upgrade.t
  }
