(* http://freenmea.net/docs *)
(* GPS *)

module Talker = struct

  let id = "GP"

  (* Fix information:
   * UTC date/time, position *)
  type gga = {
    ptime : Ptime.t ; (* POSIX time of day wrt epoch (UTC) *)
    coordinates : Coordinates.t ; (* Latitude, Longitude *)
    gps_quality : int ; (* GPS Quality Indicator (none/fix/differential
                         * fix) *)
    satellites_number : int ; (* Number of satellites in view, 00 - 12 *)
    dilution : float option ; (* Horizontal Dilution of precision *)
    antenna_altitude : float option ; (* Antenna Altitude above/below
                                       * mean-sea-level (geoid) *)
    (*
    antenna_altitude_unit : char ; (* Units of antenna altitude, meters *)
     *)
    separation : float option ; (* Geoidal separation, the difference
                                 * between the WGS-84 earth ellipsoid
                                 * and mean-sea-level (geoid), "-" means
                                 * mean-sea-level below ellipsoid *)
    (*
    separation_unit : char ; (* Units of geoidal separation, meters *)
     *)
    age : float option ; (* Age of differential GPS data, time in
                          * seconds since last SC104 type 1 or 9 update,
                          * null field when DGPS is not used *)
    station_id : int ; (* Differential reference station ID, 0000-1023 *)
    checksum : int ; (* Checksum *)
  }

  (* Recommended minimum data for gps:
   * UTC date/time, position, course, speed *)
  type rmc = {
    ptime : Ptime.t ; (* POSIX time of day (UTC) *)
    coordinates : Coordinates.t ; (* Latitude, Longitude *)
    speed : float ; (* Speed over ground, knots *)
    track : float option ; (* Track made good, degrees true *)
    variation : float option ; (* Magnetic Variation, degrees + E or W *)
    checksum : int ; (* Checksum *)
  }

  (** Talker sentences *)
  type t =
    | GGA of gga (** Fix information:
                  * UTC date/time, position *)
    | RMC of rmc (** Recommended minimum data for gps:
                  * UTC date/time, position, course, speed *)

  (* Output a talker sentence. *)
  let pp chan = function
    | GGA {ptime;coordinates;gps_quality;satellites_number;dilution;
           antenna_altitude;separation;age;station_id;checksum} ->
        Format.fprintf chan "GGA,%a,%a,%d,%d,%s,%s,M,%s,M,%s,%s*%X"
          Utils.pp_time ptime
          Coordinates.pp_nmea coordinates
          gps_quality satellites_number
          (match dilution with Some f -> string_of_float f | None -> "")
          (match antenna_altitude with Some f -> string_of_float f | None -> "")
          (match separation with Some f -> string_of_float f | None -> "")
          (match age with Some f -> string_of_float f | None -> "")
          (if station_id=0 then "" else Printf.sprintf "%.4d" station_id)
          checksum
    | RMC {ptime;coordinates;speed;track;variation;checksum} ->
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

  let to_point ~start = function
    | GGA _ -> None
    | RMC {ptime;coordinates;_} ->
      let x,y = Coordinates.to_seconds coordinates
      and z =
        match Ptime.(diff ptime start |> of_span) with
        | Some t -> Ptime.to_float_s t
        | None -> 0.
      in
      Some (Gg.V3.v x y z)

  let of_point ~start point =
    let x = Gg.V3.x point
    and y = Gg.V3.y point
    and z = Gg.V3.z point in
    match Ptime.of_float_s z with
    | None -> None
    | Some diff ->
      match Ptime.add_span start @@ Ptime.to_span diff with
      | None -> None
      | Some ptime ->
        Some (RMC {
            ptime ;
            coordinates = Coordinates.of_seconds (x,y) ;
            speed = 0. ; (* XXX *)
            track = None ;
            variation = None ;
            checksum = 0 ; (* XXX *)
          })

end

include Sentence.Make(Talker)

type segment = (Ptime.t * (Ptime.t * Ptime.t)) * t list

let pp_times fmt (time0,(time1,time2)) =
  fprintlf fmt "@Sonygps/ver5.0/wgs-84/%a/"
    Utils.pp_datetime time0 ;
  fprintlf fmt "@Sonygpsoption/0/%a/%a/"
    Utils.pp_datetime time1 Utils.pp_datetime time2

let pp_segment fmt (times,sentences) =
  pp_times fmt times ;
  List.iter (pp fmt) sentences

let trajectory_to_segment ~start =
  let aux sentences point =
    match of_point ~start point with
    | Some sentence -> sentence::sentences
    | None -> sentences
  in
  fun points ->
    (start,(start,start)),List.rev @@ List.fold_left aux [] points

let segment_to_trajectory ~start (_,sentences) =
  let aux points sentence =
    match to_point ~start sentence with
    | Some point -> point::points
    | None -> points
  in
  List.rev @@ List.fold_left aux [] sentences

let segments_to_trajectory segments =
  let ((start,_),_),_ = NEList.pop segments in
  start,
  List.flatten @@
  NEList.(map (segment_to_trajectory ~start) segments |> to_list)
