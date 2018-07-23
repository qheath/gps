(* http://freenmea.net/docs *)
(* GPS *)

module Talker = struct

  let id = "GP"

  (* Talker sentences *)
  type t =
    (* Fix information:
     * UTC date/time, position *)
    | GGA of
        (Ptime.t * (* POSIX time of day wrt epoch (UTC) *)
         Coordinates.t * (* Latitude, Longitude *)
         int * (* GPS Quality Indicator (none/fix/differential fix) *)
         int * (* Number of satellites in view, 00 - 12 *)
         float option * (* Horizontal Dilution of precision *)
         float option * (* Antenna Altitude above/below mean-sea-level
                         * (geoid) *)
         (* Units of antenna altitude, meters *)
         float option * (* Geoidal separation, the difference between the
                         * WGS-84 earth ellipsoid and mean-sea-level
                         * (geoid), "-" means mean-sea-level below
                         * ellipsoid *)
         (* Units of geoidal separation, meters *)
         float option * (* Age of differential GPS data, time in seconds
                         * since last SC104 type 1 or 9 update, null field
                         * when DGPS is not used *)
         int * (* Differential reference station ID, 0000-1023 *)
         int) (* Checksum *)

    (* Recommended minimum data for gps:
     * UTC date/time, position, course, speed *)
    | RMC of
        (Ptime.t * (* POSIX time (UTC) *)
         Coordinates.t * (* Latitude, Longitude *)
         float * (* Speed over ground, knots *)
         float option * (* Track made good, degrees true *)
         float option * (* Magnetic Variation, degrees + E or W *)
         int) (* Checksum *)

  (* Output a talker sentence. *)
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

  let to_point = function
    | GGA _ -> None
    | RMC (time,coordinates,_,_,_,_) ->
      let x,y = Coordinates.to_seconds coordinates
      and z = Ptime.to_float_s time in
      Some (Gg.V3.v x y z)

end

include Sentence.Make(Talker)

type segment = (Ptime.t * (Ptime.t * Ptime.t)) * t list

let pp_times fmt (time0,(time1,time2)) =
  fprintlf fmt "@Sonygps/ver5.0/wgs-84/%a%a/"
    Utils.pp_date time0 Utils.pp_time time0 ;
  fprintlf fmt "@Sonygpsoption/0/%a%a/%a%a/"
    Utils.pp_date time1 Utils.pp_time time1
    Utils.pp_date time2 Utils.pp_time time2

let pp_segment fmt (times,sentences) =
  pp_times fmt times ;
  List.iter (pp fmt) sentences

let segment_to_trajectory ((t0,(t1,t2)),sentences) =
  let aux points sentence =
    match to_point sentence with
    | Some point -> point::points
    | None -> points
  in
  List.rev @@ List.fold_left aux [] sentences

let segments_to_trajectory segments =
  List.flatten @@ List.map segment_to_trajectory segments
