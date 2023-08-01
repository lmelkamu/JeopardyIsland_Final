open! Core

module Upgrade = struct
  type t =
    | Double_points
    | Two_chances
    | No_loss
end

type t =
  { name : string
  ; mutable points : int
  ; mutable curr_island : Island.t
  ; mutable upgrades : Upgrade.t
  }

let equal t1 t2 = String.equal t1.name t2.name
