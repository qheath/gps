let write_sony_gps_file path segments =
  let oc = open_out path in
  let fmt = Format.formatter_of_out_channel oc in
  NEList.iter (GP.pp_segment fmt) (fun _ _ -> ()) segments ;
  close_out oc
