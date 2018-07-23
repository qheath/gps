type ns = [ `N | `S ]
type ew = [ `E | `W ]

module Coordinate = struct

  type t =
    | DD of   (float)             (* decimal degree *)
    | DDM of  (int * float)       (* degree, decimal minute *)
    | DMDS of (int * int * float) (* degree, minute, decimal second *)

  let to_dd = function
    | DD dd -> dd
    | DDM (d,dm) -> (float)d +. dm /. 60.
    | DMDS (d,m,ds) -> (float)d +. ((float)m +. ds /. 60.) /. 60.

  let to_ddm = function
    | DD dd -> int_of_float dd,mod_float (dd *. 60.) 60.
    | DDM (d,dm) -> d,dm
    | DMDS (d,m,ds) -> d,(float)m +. ds /. 60.

  let to_dmds = function
    | DD dd ->
        int_of_float dd,
        int_of_float (mod_float (dd *. 60.) 60.),
        mod_float (dd *. 3600.) 60.
    | DDM (d,dm) -> d,int_of_float dm,mod_float (dm *. 60.) 60.
    | DMDS (d,m,ds) -> d,m,ds

  let to_ds = function
    | DD dd -> dd *. 3600.
    | DDM (d,dm) -> ((float)d *. 60. +. dm) *. 60.
    | DMDS (d,m,ds) -> ((float)d *. 60. +. (float)m) *. 60. +. ds

  let pp chan = function
    | DD dd -> Format.fprintf chan "%f°" dd
    | DDM (d,dm) -> Format.fprintf chan "%d°%f'" d dm
    | DMDS (d,m,ds) -> Format.fprintf chan "%d°%d'%f''" d m ds

end

type t = (Coordinate.t * ns) * (Coordinate.t * ew)

let of_floats (latitude,longitude) =
  (Coordinate.DD (abs_float latitude),(if latitude>0. then `N else `S)),
  (Coordinate.DD (abs_float longitude),(if longitude>0. then `E else `W))

let pp_nesw chan nesw =
  let c = match nesw with
    | `N -> 'N'
    | `S -> 'S'
    | `E -> 'E'
    | `W -> 'W'
  in
  Format.pp_print_char chan c

let pp chan (latitude,longitude) =
  Format.fprintf chan "%a%a,%a%a"
    Coordinate.pp (fst latitude)
    pp_nesw (snd latitude)
    Coordinate.pp (fst longitude)
    pp_nesw (snd longitude)

let pp_nmea chan ((latitude_c,latitude_r),(longitude_c,longitude_r)) =
  let latitude_d,latitude_dm = Coordinate.to_ddm latitude_c
  and longitude_d,longitude_dm = Coordinate.to_ddm longitude_c in
  Format.fprintf chan "%d%07.4f,%a,%d%07.4f,%a"
    latitude_d latitude_dm
    pp_nesw latitude_r
    longitude_d longitude_dm
    pp_nesw longitude_r

let ns_to_second (c,r) =
  let ds = Coordinate.to_ds c in
  match r with
    | `N -> ds
    | `S -> -.ds

let ew_to_second (c,r) =
  let ds = Coordinate.to_ds c in
  match r with
    | `E -> ds
    | `W -> -.ds

let to_seconds (latitude,longitude) =
  ns_to_second latitude,ew_to_second longitude
