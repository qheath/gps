type ns = [ `N | `S ]
type ew = [ `E | `W ]

module Coordinate : sig

  type t =
    | DD of   (float)             (** decimal degree *)
    | DDM of  (int * float)       (** degree, decimal minute *)
    | DMDS of (int * int * float) (** degree, minute, decimal second *)

end

type t = (Coordinate.t * ns) * (Coordinate.t * ew)

val pp : Format.formatter -> t -> unit
val pp_nmea : Format.formatter -> t -> unit

val to_seconds : t -> float * float
val of_seconds : float * float -> t
