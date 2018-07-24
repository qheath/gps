let render_renderable ~filename ~fmt ~renderable =
  let oc = open_out filename in
  let r =
    let warn w = JupiterI.Output.eprintf "%a" Vg.Vgr.pp_warning w in
    Vg.Vgr.create ~warn (Vgr_cairo.stored_target fmt) (`Channel oc)
  in
  let () =
    match Vg.Vgr.render r (`Image renderable) with
    | `Partial -> JupiterI.Output.eprintf "%S incomplete" filename
    | `Ok -> ()
  in
  let () =
    match Vg.Vgr.render r `End with
    | `Partial -> JupiterI.Output.eprintf "%S incomplete" filename
    | `Ok -> ()
  in
  close_out oc

let fill_path ~colour path =
  let area = `Aeo in
  Vg.I.const colour |> Vg.I.cut ~area path

let stroke_path ~colour ~stroke_width path =
  let area = `O { Vg.P.o with Vg.P.width = stroke_width } in
  Vg.I.const colour |> Vg.I.cut ~area path

let render_paths ~filename ~outer_paths
    ~outer_pixel_size
    ~box_stroke_width
    ~outer_box_size
  =
  let physical_size = (* mm *)
    let outer_aspect = Gg.Size2.aspect outer_pixel_size in
    Gg.Size2.of_h ~aspect:outer_aspect 100. (* completely arbitrary *)
  in
  let fmt =
    let ppm = (* p/m *)
      Gg.Size2.v
        Gg.Size2.(w outer_pixel_size *. 1000. /. w physical_size)
        Gg.Size2.(h outer_pixel_size *. 1000. /. h physical_size)
    in
    `Png ppm
  and renderable =
    let outer_box =
      Gg.(Box2.v V2.zero outer_box_size)
    and image =
      let _,images =
        let n = (float)(NEList.length outer_paths) in
        let colour i = Gg.(V4.mix Color.red Color.blue (i/.(n-.1.))) in
        let f outer_path (i,images') =
          let image =
            stroke_path
              ~colour:(colour i) ~stroke_width:box_stroke_width outer_path
          in
          (i +. 1.),NEList.push image images'
        and g _ _ (i,images) = (i,Some images) in
        NEList.fold f g outer_paths (0.,None)
      in
      (* TODO NEList.binop Vg.I.blend @@ ... *)
      let f image1 = function
        | None -> image1
        | Some image0 -> Vg.I.blend image1 image0
      and g _ _ image = Some image in
      NEList.fold f g images (Some (Vg.I.const Gg.Color.white))
    in
    physical_size,outer_box,image
  in
  render_renderable ~filename ~fmt ~renderable

type measurements = {
  box_size : Gg.Size2.t ;
  outer_pixel_size : Gg.Size2.t ;
  margin : float ;
  stroke_width : float ;
  point_radius : float ;
}

let convert_measurements {
  box_size = inner_box_size ;
  outer_pixel_size = max_outer_pixel_size ;
  margin = pixel_margin ;
  stroke_width = pixel_stroke_width ;
  point_radius = pixel_point_radius ;
} =
  let pad_size =
    let add_to_size v size =
      Gg.(Size2.v (V2.x v +. Size2.w size) (V2.y v +. Size2.h size))
    in
    fun f size -> add_to_size Gg.V2.((2. *. f) * v 1. 1.) size
  in
  let pixel_density =
    let max_inner_pixel_size =
      pad_size (-.pixel_margin) max_outer_pixel_size
    in
    let pixel_density_w =
      Gg.Size2.((w max_inner_pixel_size) /. (w inner_box_size))
    and pixel_density_h =
      Gg.Size2.((h max_inner_pixel_size) /. (h inner_box_size))
    in
    min pixel_density_w pixel_density_h
  in
  let outer_pixel_size =
    let inner_pixel_size =
      Gg.Size2.(v (pixel_density *. w inner_box_size)
                  (pixel_density *. h inner_box_size))
    in
    pad_size pixel_margin inner_pixel_size
  and box_margin = pixel_margin /. pixel_density
  and box_stroke_width = pixel_stroke_width /. pixel_density
  and box_point_radius = pixel_point_radius /. pixel_density in
  let outer_box_size = pad_size box_margin inner_box_size in
  {
    box_size = outer_box_size ;
    outer_pixel_size = outer_pixel_size ;
    margin = box_margin ;
    stroke_width = box_stroke_width ;
    point_radius = box_point_radius ;
  }

let render_atoms atoms average'
    ~filename
    ~max_outer_pixel_size
    ~pixel_margin
    ~pixel_stroke_width
    ~pixel_point_radius
  =
  let inner_box = Atom.to_box atoms in
  let {
    box_size = outer_box_size ;
    outer_pixel_size = outer_pixel_size ;
    margin = box_margin ;
    stroke_width = box_stroke_width ;
    point_radius = box_point_radius ;
  } = convert_measurements {
      box_size = Gg.Box2.size inner_box ;
      outer_pixel_size = max_outer_pixel_size ;
      margin = pixel_margin ;
      stroke_width = pixel_stroke_width ;
      point_radius = pixel_point_radius ;
    }
  in
  let outer_paths =
    let left_path,centre_path,right_path =
      (Vg.P.empty,Vg.P.empty,Vg.P.empty)
      |> Atom.to_paths ~point_radius:box_point_radius atoms
    in
    let average_path = match average' with
      | Some average ->
        let path =
          let cluster =
            Cluster.of_points @@
            NEList.map (fun (_,p,_,_) -> Gg.V3.of_v2 p ~z:0.) average
          in
          Vg.P.empty
          |> Cluster.to_path ~point_radius:box_point_radius cluster
        in
        [path]
      | None -> []
    in
    let inner_paths =
      NEList.(push left_path (of_list (centre_path::(average_path@[right_path]))))
    in
    inner_paths
    |> NEList.map (Vg.P.tr Gg.(M3.move2 @@ V2.neg @@ Box2.o inner_box))
    |> NEList.map (Vg.P.tr Gg.(M3.move2 @@ V2.(box_margin * v 1. 1.)))
  in
  render_paths
    ~filename
    ~outer_paths
    ~outer_pixel_size
    ~box_stroke_width
    ~outer_box_size
