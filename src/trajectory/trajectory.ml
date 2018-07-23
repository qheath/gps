module Point : sig

  type t = Gg.V3.t

  (* export *)
  val to_path : radius:float -> t -> Vg.path -> Vg.path
  val pair_to_path : t -> t -> Vg.path -> Vg.path
  val to_box : t -> Gg.Box3.t

  (* misc *)
  val barycentre : (t * float) NEList.t -> (t * float)

end = struct

  type t = Gg.V3.t

  (* export *)

  let to_path ~radius point path =
    path |> Vg.P.circle (Gg.V2.of_v3 point) radius

  let pair_to_path point0 point1 path =
    path |> Vg.P.sub (Gg.V2.of_v3 point0) |> Vg.P.line (Gg.V2.of_v3 point1)

  let to_box point =
    Gg.(Box3.v_mid point Size3.zero)

  (* misc *)

  let barycentre pws =
    let q,w =
      pws
      |> NEList.map (fun (p,w) -> Gg.V3.(w * p),w)
      |> NEList.binop (fun (q0,w0) (q1,w1) -> Gg.V3.(q0 + q1),(w0 +. w1))
    in
    Gg.V3.(q / w),w

end
