module Point : sig

  type t = Gg.V3.t

  (* export *)
  val to_path : radius:float -> t -> Vg.path -> Vg.path
  val pair_to_path : t -> t -> Vg.path -> Vg.path
  val to_box : t -> Gg.Box3.t

  (* misc *)
  val barycentre : (t * float) NEList.t -> (t * float)

end = struct

  type t = Gg.V3.t

  (* export *)

  let to_path ~radius point path =
    path |> Vg.P.circle (Gg.V2.of_v3 point) radius

  let pair_to_path point0 point1 path =
    path |> Vg.P.sub (Gg.V2.of_v3 point0) |> Vg.P.line (Gg.V2.of_v3 point1)

  let to_box point =
    Gg.(Box3.v_mid point Size3.zero)

  (* misc *)

  let barycentre pws =
    let q,w =
      pws
      |> NEList.map (fun (p,w) -> Gg.V3.(w * p),w)
      |> NEList.binop (fun (q0,w0) (q1,w1) -> Gg.V3.(q0 + q1),(w0 +. w1))
    in
    Gg.V3.(q / w),w

end

(*
let _ = Random.self_init ()

let tau = atan 1. *. 8.
 *)

module Cluster : sig

  type t

  (* construction *)
  val of_point : Point.t -> t
  val of_points : Point.t NEList.t -> t
  val multi_merge : t NEList.t -> t

  (* deconstruction *)
  val to_point : t -> Point.t
  val to_points : t -> Point.t NEList.t
  val pair_to_points : t -> t -> Point.t * Point.t
  val split : t -> (t * t) option
  val fold :
    (Point.t -> 'start -> 'finish) ->
    (Point.t -> Point.t -> 'finish -> 'start) ->
    t -> 'start -> 'finish
  val binop :
    (Point.t -> 'a) ->
    ('a -> 'a -> 'a) ->
    t -> 'a

  (* export *)
  val to_path : point_radius:float -> t -> Vg.path -> Vg.path
  val to_box : t -> Gg.Box3.t

  (* misc *)
  val size : t -> int
  val score : t NEList.t -> float
  val middle : t -> t -> Point.t

end = struct

  type inner_cluster = Point.t * int * subclusters
  and subclusters =
    | Point
    | SubClusters of (inner_cluster * Point.t) * (Point.t * inner_cluster)
  type t = Point.t * inner_cluster * Point.t

  (* construction *)

  let of_point p = p,(p,1,Point),p

  let merge
      (start0,(point0,n0,_ as inner0),end0)
      (start1,(point1,n1,_ as inner1),end1) =
    let point =
      Gg.V3.(mix point0 point1 (((float)n1) /. ((float)n0 +. (float)n1)))
    and n = n0 + n1
    and subclusters = SubClusters ((inner0,end0),(start1,inner1)) in
    start0,(point,n,subclusters),end1

  let multi_merge =
    let pass forward =
      let rec aux backward0 forward1 =
        let cluster0,backward0' = NEList.pop backward0
        and cluster1,forward1' = NEList.pop forward1 in
        let cluster01 =
          if forward then merge cluster0 cluster1
          else merge cluster1 cluster0
        in
        let backward1 = NEList.push cluster01 backward0' in
        match forward1' with
        | None -> backward1
        | Some forward2 ->
          let cluster2,forward2' = NEList.pop forward2 in
          let backward2 = NEList.push cluster2 (Some backward1) in
          match forward2' with
          | None -> backward2
          | Some forward3 -> aux backward2 forward3
      in
      aux
    in
    let rec aux forward clusters0 =
      let cluster0,clusters0' = NEList.pop clusters0 in
      match clusters0' with
      | None -> cluster0
      | Some clusters1 ->
        let clusters2 = pass forward (NEList.push cluster0 None) clusters1 in
        aux (not forward) clusters2
    in
    aux true

  let of_points points =
    multi_merge @@ NEList.map of_point points

  (* deconstruction *)

  let to_point (_,(p,_,_),_) = p

  let split (start0,(_,_,subclusters),end1) =
    match subclusters with
    | Point -> None
    | SubClusters ((inner0,end0),(start1,inner1)) ->
      Some ((start0,inner0,end0),(start1,inner1,end1))

  let pop =
    let rec aux clusters =
      let cluster,clusters' = NEList.pop clusters in
      match split cluster with
      | None -> to_point cluster,clusters'
      | Some (cluster0,cluster1) ->
        aux NEList.(push cluster0 (Some (push cluster1 clusters')))
    in
    aux

  let fold f g =
    let rec aux (accum,point0) = function
      | None -> accum
      | Some clusters ->
        let point1,clusters' = pop clusters in
        let accum' = g point0 point1 accum in
        aux (f point1 accum',point1) clusters'
    in
    fun cluster seed ->
      let point,clusters' = pop (NEList.push cluster None) in
      aux (f point seed,point) clusters'

  let binop map op =
    let f point = function None -> map point | Some x -> op x @@ map point
    and g _ _ x = Some x in
    fun cluster -> fold f g cluster None

  let to_points =
    let f point points' = NEList.push point points'
    and g _ _ points = Some points in
    fun cluster -> NEList.rev @@ fold f g cluster None

  let pair_to_points (_,(_,_,_),end0) (start1,(_,_,_),_) =
    end0,start1

  (* export *)

  let to_path ~point_radius =
    fold (Point.to_path ~radius:point_radius) Point.pair_to_path

  let to_box =
    binop Point.to_box Gg.Box3.union

  (* misc *)

  (*
  let generate_example ?(dx=tau) n =
    let make_ideal_point t = Gg.V2.v t (sin t) in
    let make_real_point ~n ~noise:(noise_speed_base,noise_radius) =
      let noise_speed = Random.float noise_speed_base +. noise_speed_base /. 2.
      and noise_offset = Random.float tau in
      fun i ->
        let t = (float)(max 0 (min i n)) /. ((float) n) *. dx in
        let ideal_point = make_ideal_point t
        and noise =
          let noise_angle = noise_speed *. t +. noise_offset in
          Gg.V2.(of_polar @@ v noise_radius noise_angle)
        and subnoise =
          let subnoise_radius = Random.float noise_radius
          and subnoise_angle = Random.float tau in
          Gg.V2.(of_polar @@ v subnoise_radius subnoise_angle)
        in
        Gg.(V3.of_v2 V2.(ideal_point + noise + subnoise) ~z:t)
    in
    let points =
      NEList.init (n+1)
        (make_real_point ~n ~noise:(2.,dx/.((float)n) /. 2.))
    in
    of_points points
   *)

  let pp fmt cluster =
    let f point () = Format.fprintf fmt "%a" Gg.V3.pp point
    and g _ _ () = () in
    fold f g cluster ()

  (*
  let total_length css =
    let f _ sum = sum
    and g p0 p1 sum = sum +. Gg.V2.(norm (p0 - p1)) in
    NEList.fold f g
      (NEList.flatten @@ NEList.map (fun (c,_) -> to_points c) css)
      0.
   *)

  let size (_,(_,n,_),_) = n

  (* TODO
   * have three ways to compute the score of a cluster sequence:
   * - sum of the distances between the barycentres
   * - sum of the distances between ends and starts
   * - a mix of both
   * and test their efficiency
  *)
  let score =
    let f _ sum = sum
    and g cluster0 cluster1 sum =
      Gg.V2.(norm2 ((of_v3 @@ to_point cluster0) - (of_v3 @@ to_point cluster1))) +. sum
    in
    fun atoms ->
      let sum = NEList.fold f g atoms 0. in
      sum *.sum *. sum

  let middle (_,_,end0) (start2,_,_) =
    let points = NEList.(push (end0,1.) (Some (push (start2,1.) None))) in
    let centre,_ = Point.barycentre points in
    centre

end

module Atom : sig

  type switch = Left | Right
  type t = Cluster.t * switch

  (* export *)
  val to_paths :
    point_radius:float -> t NEList.t -> (Vg.path * Vg.path * Vg.path) ->
    Vg.path * Vg.path * Vg.path
  val to_box : t NEList.t -> Gg.Box2.t

  (* misc *)
  val binop :
    (Cluster.t -> 'a) ->
    ('a -> 'a -> 'a) ->
    t NEList.t -> 'a
  val max_size : t NEList.t -> int
  val better : t NEList.t -> t NEList.t -> bool

end = struct

  type switch = Left | Right
  type t = Cluster.t * switch

  let to_paths ~point_radius =
    let f (cluster,switch) (left_path,centre_path,right_path) =
      match switch with
      | Left ->
        ((left_path |> Cluster.to_path ~point_radius cluster),
         centre_path,
         right_path)
      | Right ->
        (left_path,
         centre_path,
         (right_path |> Cluster.to_path ~point_radius cluster))
    and g (cluster0,_) (cluster1,_) (left_path,centre_path,right_path) =
      let end0,start1 = Cluster.pair_to_points cluster0 cluster1 in
      left_path,
      centre_path |> Point.pair_to_path end0 start1,
      right_path
    in
    NEList.fold f g

  let binop map op atoms =
    atoms
    |> NEList.map (fun (cluster,_) -> map cluster)
    |> NEList.binop op

  let to_box =
    let f cluster =
      let box3 = Cluster.to_box cluster in
      Gg.(Box2.of_pts (V2.of_v3 @@ Box3.min box3) (V2.of_v3 @@ Box3.max box3))
    in
    binop f Gg.Box2.union

  (* misc *)

  let max_size =
    binop Cluster.size max

  (* TODO
   * have a parameter indicating how much this function is
   * randomised
   *)
  let better atoms0 atoms1 =
    let score0 = Cluster.score @@ NEList.map fst atoms0
    and score1 = Cluster.score @@ NEList.map fst atoms1 in
    (*
    Random.float (score0+.score1)<score1
     *)
    score0<score1

end

module Render = struct

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
              NEList.map (fun (_,p,_) -> Gg.V3.of_v2 p ~z:0.) average
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

end
