type switch = Left | Right
type t = Cluster.t * switch

(* export *)
val to_paths :
  point_radius:float -> t NEList.t -> (Vg.path * Vg.path * Vg.path) ->
  Vg.path * Vg.path * Vg.path
val to_box : t NEList.t -> Gg.Box2.t
val pp : Format.formatter -> t -> unit

(* misc *)
val binop :
  (Cluster.t -> 'a) ->
  ('a -> 'a -> 'a) ->
  t NEList.t -> 'a
val max_size : t NEList.t -> float
val better : t NEList.t -> t NEList.t -> bool
