type ns = [ `N | `S ]
type ew = [ `E | `W ]

type coordinate =
  | DD of   (float)             (** decimal degree *)
  | DDM of  (int * float)       (** degree, decimal minute *)
  | DMDS of (int * int * float) (** degree, minute, decimal second *)
type t = (coordinate * ns) * (coordinate * ew)

val pp_nmea : Format.formatter -> t -> unit
