type t

(** {6 Construction} *)

val of_point : Point.t3 -> t
val of_points : Point.t3 NEList.t -> t

(** balanced cluster merge *)
val multi_merge : t NEList.t -> t

(** {6 Deconstruction} *)

val to_point : t -> Point.t3
val to_points : t -> Point.t3 NEList.t
val pair_to_points : t -> t -> Point.t3 * Point.t3
val split : t -> (t * t) option
val fold :
  (Point.t3 -> 'start -> 'finish) ->
  (Point.t3 -> Point.t3 -> 'finish -> 'start) ->
  t -> 'start -> 'finish
val binop :
  (Point.t3 -> 'a) ->
  ('a -> 'a -> 'a) ->
  t -> 'a

(** {6 Export} *)

val to_path : point_radius:float -> t -> Vg.path -> Vg.path
val to_box : t -> Gg.Box3.t
val pp : Format.formatter -> t -> unit
val pp_full : Format.formatter -> t -> unit

(** {6 Misc} *)

val size : t -> float
val score : t NEList.t -> float
val middle : t -> t -> Point.t3
