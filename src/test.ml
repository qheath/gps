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

let build_atoms ofile =
  let aux (name,switch) atoms' =
    match NMEA.Input.read_sony_gps_file @@ ofile name with
    | None ->
      JupiterI.Output.wprintf "cannot read %s, ignoring" name ;
      atoms'
    | Some segments ->
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

let copy_names ifile ofile =
  let copy name =
    match NMEA.Input.read_sony_gps_file @@ ifile name with
    | None ->
      JupiterI.Output.wprintf "cannot read %s, ignoring" name
    | Some segments ->
      NMEA.Output.write_sony_gps_file (ofile name) segments
  in
  List.iter copy

let main verbosity unverbosity data_dir input_names build_frames =
  List.iter (fun _ -> JupiterI.Output.Verbosity.moreTalk ()) verbosity ;
  List.iter (fun _ -> JupiterI.Output.Verbosity.lessTalk ()) unverbosity ;
  let ofile = Printf.sprintf "%s/%s/out.nmea" data_dir in
  if build_frames then begin
    (* make frames out of "out" *)
    match prepare_names input_names with
    | None -> ()
    | Some prepared_names ->
      match build_atoms ofile prepared_names with
      | None -> ()
      | Some atoms ->
        let _atoms,average' =
          Trajectory.Interleave.process
            ~odir:(Printf.sprintf "%s/frames" data_dir) atoms
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
  end else begin
    (* copy "in" into "out" *)
    let ifile = Printf.sprintf "%s/%s/in.nmea" data_dir in
    copy_names ifile ofile input_names
  end

let () =
  let verbosity =
    let doc = "increase verbosity" in
    Cmdliner.Arg.(value & flag_all & info ["v";"verbose"] ~doc)
  and unverbosity =
    let doc = "decrease verbosity" in
    Cmdliner.Arg.(value & flag_all & info ["q";"quiet"] ~doc)
  and data_dir =
    let doc = "where to read from and write to" in
    Cmdliner.Arg.(value & opt dir "data" & info ["d";"data_dir"] ~docv:"dir_path" ~doc)
  and input_names =
    let doc = "what to read" in
    Cmdliner.Arg.(value & opt_all string [] & info ["i";"input"] ~docv:"name" ~doc)
  and build_frames =
    let doc = "build frames instead of copying NMEA files" in
    Cmdliner.Arg.(value & flag & info ["f";"frames"] ~doc)
  in
  let term =
    Cmdliner.Term.(const main $ verbosity $ unverbosity $ data_dir $ input_names $ build_frames)
  in
  Stdlib.exit @@ Cmdliner.Cmd.(eval (v (info "gps") term))
