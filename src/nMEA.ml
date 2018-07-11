(****************************************************************************)
(* NMEA GPS data                                                            *)
(* Copyright (C) 2018 Quentin Heath                                         *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU General Public License as published by     *)
(* the Free Software Foundation, either version 3 of the License, or        *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU General Public License for more details.                             *)
(*                                                                          *)
(* You should have received a copy of the GNU General Public License        *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(****************************************************************************)

let pp_date chan ptime =
  let y,m,d = Ptime.to_date ptime in
  Format.fprintf chan "%.2d%.2d%.2d" d m (y mod 100)

let pp_time chan ptime =
  let _,((h,m,s),_) = Ptime.to_date_time ptime
  and frac = Ptime.Span.to_float_s @@ Ptime.frac_s ptime in
  Format.fprintf chan "%.2d%.2d%06.3f" h m ((float)s +. frac)

let pp_message_description chan = function
  | Types.GP.GGA (ptime,coordinates,quality,number,dilution,
         altitude,separation,age,reference,checksum) ->
      Format.fprintf chan "GGA,%a,%a,%d,%d,%s,%s,M,%s,M,%s,%s*%X"
        pp_time ptime
        Types.Coordinates.pp_nmea coordinates
        quality number
        (match dilution with Some f -> string_of_float f | None -> "")
        (match altitude with Some f -> string_of_float f | None -> "")
        (match separation with Some f -> string_of_float f | None -> "")
        (match age with Some f -> string_of_float f | None -> "")
        (if reference=0 then "" else Printf.sprintf "%.4d" reference)
        checksum
  | Types.GP.RMC (ptime,coordinates,speed,track,variation,checksum) ->
      Format.fprintf chan "RMC,%a,A,%a,%.2f,%s,%a,%s,%s,A*%X"
        pp_time ptime
        Types.Coordinates.pp_nmea coordinates
        speed
        (match track with Some f -> string_of_float f | None -> "")
        pp_date ptime
        (match variation with
           | Some f -> string_of_float (abs_float f)
           | None -> "")
        (match variation with
           | Some f -> if f>0. then "N" else "S"
           | None -> "")
        checksum

let pp_talker chan (message_description) =
  Format.fprintf chan "%s%a"
    Types.GP.id
    pp_message_description message_description

let pp_proprietary chan () =
  Format.fprintf chan ""

let pp_query chan () =
  Format.fprintf chan ""

let fprintlf chan f =
  Format.fprintf chan (f ^^ "\r\n%!")

let pp_sentence chan =
  let aux f = fprintlf chan "$%a" f in
  function
    | Types.GP.Talker (message_description) ->
        aux pp_talker (message_description)
    | Types.GP.Proprietary -> aux pp_proprietary ()
    | Types.GP.Query -> aux pp_query ()
