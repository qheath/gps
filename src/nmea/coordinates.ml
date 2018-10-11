type ns = [ `N | `S ]
type ew = [ `E | `W ]

module Coordinate = struct

  type t =
    | DD of   (float)             (* decimal degree *)
    | DDM of  (int * float)       (* degree, decimal minute *)
    | DMDS of (int * int * float) (* degree, minute, decimal second *)

  (*
  let to_dd = function
    | DD dd -> dd
    | DDM (d,dm) -> (float)d +. dm /. 60.
    | DMDS (d,m,ds) -> (float)d +. ((float)m +. ds /. 60.) /. 60.
   *)

  let to_ddm = function
    | DD dd -> int_of_float dd,mod_float (dd *. 60.) 60.
    | DDM (d,dm) -> d,dm
    | DMDS (d,m,ds) -> d,(float)m +. ds /. 60.

  (*
  let to_dmds = function
    | DD dd ->
        int_of_float dd,
        int_of_float (mod_float (dd *. 60.) 60.),
        mod_float (dd *. 3600.) 60.
    | DDM (d,dm) -> d,int_of_float dm,mod_float (dm *. 60.) 60.
    | DMDS (d,m,ds) -> d,m,ds
   *)

  let to_ds = function
    | DD dd -> dd *. 3600.
    | DDM (d,dm) -> ((float)d *. 60. +. dm) *. 60.
    | DMDS (d,m,ds) -> ((float)d *. 60. +. (float)m) *. 60. +. ds

  let of_ds ds =
    DD (ds /. 3600.)

  let pp fmt = function
    | DD dd -> Format.fprintf fmt "%f°" dd
    | DDM (d,dm) -> Format.fprintf fmt "%d°%f'" d dm
    | DMDS (d,m,ds) -> Format.fprintf fmt "%d°%d'%f''" d m ds

  let ddm f =
    let degrees = (int_of_float f)/100 in
    let minutes = f -. ((float)(100 * degrees)) in
    DDM (degrees,minutes)

end

type t = (Coordinate.t * ns) * (Coordinate.t * ew)

let pp_nesw fmt nesw =
  let c = match nesw with
    | `N -> 'N'
    | `S -> 'S'
    | `E -> 'E'
    | `W -> 'W'
  in
  Format.pp_print_char fmt c

let pp fmt (latitude,longitude) =
  Format.fprintf fmt "%a%a,%a%a"
    Coordinate.pp (fst latitude)
    pp_nesw (snd latitude)
    Coordinate.pp (fst longitude)
    pp_nesw (snd longitude)

let pp_nmea fmt ((latitude_c,latitude_r),(longitude_c,longitude_r)) =
  let latitude_d,latitude_dm = Coordinate.to_ddm latitude_c
  and longitude_d,longitude_dm = Coordinate.to_ddm longitude_c in
  Format.fprintf fmt "%d%07.4f,%a,%d%07.4f,%a"
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

let ns_of_second ds =
  Coordinate.of_ds @@ abs_float ds,
  if ds>0. then `N else `S

let ew_of_second ds =
  Coordinate.of_ds @@ abs_float ds,
  if ds>0. then `E else `W

let of_seconds (latitude,longitude) =
  ns_of_second latitude,ew_of_second longitude
