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

let dir = ref "data"

let ifile name =
  Printf.sprintf "%s/%s/in.nmea" !dir name
and ofile name =
  Printf.sprintf "%s/%s/out.nmea" !dir name
and odir () =
  Printf.sprintf "%s/frames" !dir

let parse_args () =
  let names = ref [] in

  let longopts = GetArg.[
    ('v',"verbose"," increase verbosity"),
    Lone JupiterI.Output.Verbosity.moreTalk ;

    ('q',"quiet"," decrease verbosity"),
    Lone JupiterI.Output.Verbosity.lessTalk ;

    ('d',"directory",Printf.sprintf "directory where to read and write [%s]" !dir),
    set_string dir ;

    ('i',"input","name what to read (multiple)"),
    (Mandatory (String (fun s -> names := s :: !names))) ;
  ] and usage =
    Printf.sprintf
      "usage: %s [<options>]"
      Sys.argv.(0)
  in

  GetArg.parse longopts ignore usage ;
  !names

let prepare_names = function
  | [] ->
    JupiterI.Output.wprintf "nothing to do, leaving" ;
    None
  | [name] ->
    let ns = name,Trajectory.Atom.Left in
    NEList.of_list [ns]
  | [name0;name1] ->
    let ns0 = name0,Trajectory.Atom.Left
    and ns1 = name1,Trajectory.Atom.Right in
    NEList.of_list [ns0;ns1]
  | names ->
    JupiterI.Output.eprintf "too much to do (%d), leaving"
      (List.length names) ;
    None

let build_atoms =
  let aux (name,switch) atoms' =
    match NMEA.Input.read_sony_gps_file @@ ifile name with
    | None ->
      JupiterI.Output.wprintf "cannot read %s, ignoring" name ;
      atoms'
    | Some segments ->
      NMEA.Output.write_sony_gps_file (ofile name) segments ;
      let _start,trajectory = NMEA.GP.segments_to_trajectory segments in
      match NEList.of_list trajectory with
      | None -> atoms'
      | Some points ->
        let atoms =
          NEList.push (Trajectory.Cluster.of_points points,switch) atoms'
        in
        Some atoms
  in
  fun prepared_names ->
    NEList.fold aux (fun _ _ atoms' -> atoms') prepared_names None

let () =
  let names = parse_args () in
  match prepare_names names with
  | None -> ()
  | Some prepared_names ->
    match build_atoms prepared_names with
    | None -> ()
    | Some atoms ->
      let _atoms,average' =
        Trajectory.Interleave.process ~odir:(odir ()) atoms
      in
      match average' with
      | None -> ()
      | Some average ->
        let print (t0,p,t1,switch1) =
          let t_left,t_right = match switch1 with
            | Trajectory.Atom.Left -> t1,t0
            | Trajectory.Atom.Right -> t0,t1
          in
          Format.eprintf "%a: %.2f -- %.2f@." Gg.V2.pp p t_left t_right
        in
        NEList.iter print (fun _ _ -> ()) average
