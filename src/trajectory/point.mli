type t3 = Gg.V3.t

(** {6 Export} *)

val to_path : radius:float -> t3 -> Vg.path -> Vg.path
val pair_to_path : t3 -> t3 -> Vg.path -> Vg.path
val to_box : t3 -> Gg.Box3.t
val pp : Format.formatter -> t3 -> unit

(** {6 Misc} *)

type t4 = Gg.V4.t

val v4_of_v3 : t3 -> t4
val v3_of_v4 : t4 -> t3

val barycentre : t4 NEList.t -> t4
val middle : t3 -> t3 -> t3
