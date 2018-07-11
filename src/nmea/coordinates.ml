type ns = [ `N | `S ]
type ew = [ `E | `W ]

type coordinate =
  | DD of   (float)             (* decimal degree *)
  | DDM of  (int * float)       (* degree, decimal minute *)
  | DMDS of (int * int * float) (* degree, minute, decimal second *)
type t = (coordinate * ns) * (coordinate * ew)

let of_floats (latitude,longitude) =
  (DD (abs_float latitude),(if latitude>0. then `N else `S)),
  (DD (abs_float longitude),(if longitude>0. then `E else `W))

let pp_nesw chan nesw =
  let c = match nesw with
    | `N -> 'N'
    | `S -> 'S'
    | `E -> 'E'
    | `W -> 'W'
  in
  Format.pp_print_char chan c

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

let pp_coordinate chan = function
  | DD dd -> Format.fprintf chan "%f°" dd
  | DDM (d,dm) -> Format.fprintf chan "%d°%f'" d dm
  | DMDS (d,m,ds) -> Format.fprintf chan "%d°%d'%f''" d m ds

let pp chan (latitude,longitude) =
  Format.fprintf chan "%a%a,%a%a"
    pp_coordinate (fst latitude)
    pp_nesw (snd latitude)
    pp_coordinate (fst longitude)
    pp_nesw (snd longitude)

let pp_nmea chan ((latitude_c,latitude_r),(longitude_c,longitude_r)) =
  let latitude_d,latitude_dm = to_ddm latitude_c
  and longitude_d,longitude_dm = to_ddm longitude_c in
  Format.fprintf chan "%d%07.4f,%a,%d%07.4f,%a"
    latitude_d latitude_dm
    pp_nesw latitude_r
    longitude_d longitude_dm
    pp_nesw longitude_r

let ns_to_float (c,r) =
  let dd = to_dd c in
  match r with
    | `N -> dd
    | `S -> -.dd

let ew_to_float (c,r) =
  let dd = to_dd c in
  match r with
    | `E -> dd
    | `W -> -.dd

let to_floats (latitude,longitude) =
  ns_to_float latitude,ew_to_float longitude
