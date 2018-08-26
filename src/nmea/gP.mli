(* http://freenmea.net/docs *)
(* GPS *)

module Talker : sig

  (** Fix information:
   * UTC date/time, position *)
  type gga = {
    ptime : Ptime.t ;
    (** POSIX time of day wrt epoch (UTC) *)

    coordinates : Coordinates.t ;
    (** Latitude, Longitude *)

    gps_quality : int ;
    (** GPS Quality Indicator (none/fix/differential fix) *)

    satellites_number : int ;
    (** Number of satellites in view, 00 - 12 *)

    dilution : float option ;
    (** Horizontal Dilution of precision *)

    antenna_altitude : float option ;
    (** Antenna Altitude above/below mean-sea-level (geoid) *)

    (*
    antenna_altitude_unit : char ;
    (** Units of antenna altitude, meters *)
     *)

    separation : float option ;
    (** Geoidal separation, the difference between the WGS-84 earth
     * ellipsoid and mean-sea-level (geoid), "-" means mean-sea-level
     * below ellipsoid *)

    (*
    separation_unit : char ;
    (** Units of geoidal separation, meters *)
     *)

    age : float option ;
    (** Age of differential GPS data, time in seconds since last SC104
     * type 1 or 9 update, null field when DGPS is not used *)

    station_id : int ;
    (** Differential reference station ID, 0000-1023 *)

    checksum : int ;
    (** Checksum *)
  }

  (** Recommended minimum data for gps:
   * UTC date/time, position, course, speed *)
  type rmc = {
    ptime : Ptime.t ;
    (** POSIX time of day (UTC) *)

    coordinates : Coordinates.t ;
    (** Latitude, Longitude *)

    speed : float ;
    (** Speed over ground, knots *)

    track : float option ;
    (** Track made good, degrees true *)

    variation : float option ;
    (** Magnetic Variation, degrees + E or W *)

    checksum : int ;
    (** Checksum *)
  }

  (** Talker sentences *)
  type t =
    | GGA of gga (** Fix information:
                  * UTC date/time, position *)
    | RMC of rmc (** Recommended minimum data for gps:
                  * UTC date/time, position, course, speed *)

  val to_point : start:Ptime.t -> t -> Gg.V3.t option

  val of_point : start:Ptime.t -> Gg.V3.t -> t option

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

val trajectory_to_segment : start:Ptime.t -> Gg.V3.t list -> segment

val segments_to_trajectory : segment NEList.t -> Ptime.t * Gg.V3.t list
