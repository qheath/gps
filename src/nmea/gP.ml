(* http://freenmea.net/docs *)
(* GPS *)

module Talker = struct

  let id = "GP"

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

  (** Output a talker sentene. *)
  let pp chan = function
    | GGA (ptime,coordinates,quality,number,dilution,
           altitude,separation,age,reference,checksum) ->
        Format.fprintf chan "GGA,%a,%a,%d,%d,%s,%s,M,%s,M,%s,%s*%X"
          Utils.pp_time ptime
          Coordinates.pp_nmea coordinates
          quality number
          (match dilution with Some f -> string_of_float f | None -> "")
          (match altitude with Some f -> string_of_float f | None -> "")
          (match separation with Some f -> string_of_float f | None -> "")
          (match age with Some f -> string_of_float f | None -> "")
          (if reference=0 then "" else Printf.sprintf "%.4d" reference)
          checksum
    | RMC (ptime,coordinates,speed,track,variation,checksum) ->
        Format.fprintf chan "RMC,%a,A,%a,%.2f,%s,%a,%s,%s,A*%X"
          Utils.pp_time ptime
          Coordinates.pp_nmea coordinates
          speed
          (match track with Some f -> string_of_float f | None -> "")
          Utils.pp_dmy ptime
          (match variation with
             | Some f -> string_of_float (abs_float f)
             | None -> "")
          (match variation with
             | Some f -> if f>0. then "N" else "S"
             | None -> "")
          checksum

end

include Sentence.Make(Talker)

type segment = (Ptime.t * (Ptime.t * Ptime.t)) * t list
