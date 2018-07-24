(* http://freenmea.net/docs *)
(* GPS *)

module Talker : sig

  (** Talker sentences *)
  type t =
    (** Fix information:
      * UTC date/time, position *)
    | GGA of
        (Ptime.t * (** POSIX time of day wrt epoch (UTC) *)
         Coordinates.t * (** Latitude, Longitude *)
         int * (** GPS Quality Indicator (none/fix/differential fix) *)
         int * (** Number of satellites in view, 00 - 12 *)
         float option * (** Horizontal Dilution of precision *)
         float option * (** Antenna Altitude above/below mean-sea-level
                          * (geoid) *)
         (** Units of antenna altitude, meters *)
         float option * (** Geoidal separation, the difference between the
                          * WGS-84 earth ellipsoid and mean-sea-level
                          * (geoid), "-" means mean-sea-level below
                          * ellipsoid *)
         (** Units of geoidal separation, meters *)
         float option * (** Age of differential GPS data, time in seconds
                          * since last SC104 type 1 or 9 update, null field
                          * when DGPS is not used *)
         int * (** Differential reference station ID, 0000-1023 *)
         int) (** Checksum *)

    (** Recommended minimum data for gps:
      * UTC date/time, position, course, speed *)
    | RMC of
        (Ptime.t * (** POSIX time (UTC) *)
         Coordinates.t * (** Latitude, Longitude *)
         float * (** Speed over ground, knots *)
         float option * (** Track made good, degrees true *)
         float option * (** Magnetic Variation, degrees + E or W *)
         int) (** Checksum *)

  val to_point : start:Ptime.t -> t -> Gg.V3.t option

end

type t =
  | Talker of Talker.t
  | Proprietary
  | Query

val fprintlf :
  Format.formatter -> ('a, Format.formatter, unit) format -> 'a

val pp : Format.formatter -> t -> unit

type segment = (Ptime.t * (Ptime.t * Ptime.t)) * t list

val pp_times :
  Format.formatter -> (Ptime.t * (Ptime.t * Ptime.t)) -> unit

val pp_segment : Format.formatter -> segment -> unit

val segments_to_trajectory : segment NEList.t -> Ptime.t * Gg.V3.t list
