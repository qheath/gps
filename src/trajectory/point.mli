type t = Gg.V3.t

(** {6 Export} *)

val to_path : radius:float -> t -> Vg.path -> Vg.path
val pair_to_path : t -> t -> Vg.path -> Vg.path
val to_box : t -> Gg.Box3.t
val pp : Format.formatter -> t -> unit

(** {6 Misc} *)

val barycentre : (t * float) NEList.t -> (t * float)
