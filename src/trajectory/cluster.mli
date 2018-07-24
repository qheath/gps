type t

(* construction *)
val of_point : Point.t -> t
val of_points : Point.t NEList.t -> t
val multi_merge : t NEList.t -> t

(* deconstruction *)
val to_point : t -> Point.t
val to_points : t -> Point.t NEList.t
val pair_to_points : t -> t -> Point.t * Point.t
val split : t -> (t * t) option
val fold :
  (Point.t -> 'start -> 'finish) ->
  (Point.t -> Point.t -> 'finish -> 'start) ->
  t -> 'start -> 'finish
val binop :
  (Point.t -> 'a) ->
  ('a -> 'a -> 'a) ->
  t -> 'a

(* export *)
val to_path : point_radius:float -> t -> Vg.path -> Vg.path
val to_box : t -> Gg.Box3.t
val pp : Format.formatter -> t -> unit

(* misc *)
val size : t -> int
val score : t NEList.t -> float
val middle : t -> t -> Point.t
