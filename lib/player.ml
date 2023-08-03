open! Core

(* module Upgrade = struct type t = | Double_points | Two_chances | No_loss
   end *)

type t =
  { name : string
  ; mutable points : int
  ; mutable curr_island : Island.t (* ; mutable upgrades : Upgrade.t *)
  }
[@@deriving sexp]

let create ~name ~island = { name; points = 0; curr_island = island }
let equal t1 t2 = String.equal t1.name t2.name
