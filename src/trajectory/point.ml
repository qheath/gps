type t3 = Gg.V3.t

(* export *)

let to_path ~radius point path =
  path |> Vg.P.circle (Gg.V2.of_v3 point) radius

let pair_to_path point0 point1 path =
  path |> Vg.P.sub (Gg.V2.of_v3 point0) |> Vg.P.line (Gg.V2.of_v3 point1)

let to_box point =
  Gg.(Box3.v_mid point Size3.zero)

let pp = Gg.V3.pp

(* misc *)

type t4 = Gg.V4.t

let v4_of_v3 = Gg.V4.of_v3 ~w:1.

let v3_of_v4 v4 =
  let x,y,z,_ = Gg.V4.to_tuple @@ Gg.V4.homogene v4 in
  Gg.V3.of_tuple (x,y,z)

let barycentre = NEList.binop Gg.V4.(+)

let middle point0 point1 =
  NEList.(push point0 (Some (push point1 None)))
  |> NEList.map v4_of_v3
  |> barycentre
  |> v3_of_v4
