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

let parse_args () =
  let ifile = ref "data/in.nmea"
  and ofile = ref "data/out.nmea"
  and delay = ref 0 in

  let longopts = GetArg.[
    ('i',"input",Printf.sprintf "file where to read [%s]" !ifile),
    set_string ifile ;

    ('o',"output",Printf.sprintf "file where to write [%s]" !ofile),
    set_string ofile ;

    ('d',"delay",Printf.sprintf "seconds how much to shift forward [%d]" !delay),
    set_int delay ;
  ] and usage =
    Printf.sprintf
      "usage: %s [<options>]"
      Sys.argv.(0)
  in

  GetArg.parse longopts ignore usage ;
  !ifile,!ofile,!delay


let () =
  let ifile,ofile,_delay = parse_args () in
  match
    let ic = open_in ifile in
    let parser,lexer,parse =
      NMEA.(Parser.Incremental.sony_gps_file,Lexer.token,Input.parse)
    and lexbuf = Lexing.from_channel ic in
    let segments =
      match parse ~parser ~lexer lexbuf with
        | Yes segments -> Some segments
        | Parser (state,position) ->
            (*
            let message =
              try Some (Parser_messages.message (Interp.number state))
              with Not_found -> None
            in
             *)
            Format.eprintf "%a: parser state %d reached, cannot go forward@."
              Input.Pos.pp position state ;
            None
        | Lexer (message) ->
            Format.eprintf "%a: lexing error: %S@."
              Input.Pos.pp (Input.Pos.of_lexbuf lexbuf ()) message ;
            None
    in
    close_in ic ;
    segments
  with
    | None -> ()
    | Some segments ->
        let oc = open_out ofile in
        let fmt = Format.formatter_of_out_channel oc in
        let aux ((time0,(time1,time2)),sentences) =
          NMEA.GP.fprintlf fmt "/%a%a/"
            NMEA.Utils.pp_date time0 NMEA.Utils.pp_time time0 ;
          NMEA.GP.fprintlf fmt "/%a%a/%a%a/"
            NMEA.Utils.pp_date time1 NMEA.Utils.pp_time time1
            NMEA.Utils.pp_date time2 NMEA.Utils.pp_time time2 ;
          List.iter (NMEA.GP.pp fmt) sentences
        in
        List.iter aux segments ;
        close_out oc ;
        ()
