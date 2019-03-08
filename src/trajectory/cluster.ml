(*
let () = Random.self_init ()
let tau = atan 1. *. 8.
 *)

type inner_cluster = Point.t4 * subclusters
and subclusters =
  | Point
  | SubClusters of (inner_cluster * Point.t3) * (Point.t3 * inner_cluster)
type t = Point.t3 * inner_cluster * Point.t3

(* construction *)

let of_point p = p,(Point.v4_of_v3 p,Point),p

let merge
    (start0,(point0,_ as inner0),end0)
    (start1,(point1,_ as inner1),end1) =
  let point = Gg.V4.(point0 + point1)
  and subclusters = SubClusters ((inner0,end0),(start1,inner1)) in
  start0,(point,subclusters),end1

let multi_merge =
  let pass forward =
    (* merge the heads of backward0 and forward1 *)
    let rec aux backward0 forward1 =
      let cluster0,backward0' = NEList.pop backward0
      and cluster1,forward1' = NEList.pop forward1 in
      let cluster01 =
        if forward then merge cluster0 cluster1
        else merge cluster1 cluster0
      in
      let backward1 = NEList.push cluster01 backward0' in
      match forward1' with
      | None ->
        (* there was an even number of clusters, return all the pairs *)
        backward1
      | Some forward2 ->
        let cluster2,forward2' = NEList.pop forward2 in
        let backward2 = NEList.push cluster2 (Some backward1) in
        match forward2' with
        | None ->
          (* there was an off number of clusters, return all pairs plus
           * the last one *)
          backward2
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

let to_point (_,(p,_),_) = Point.v3_of_v4 p

let split (start0,(_,subclusters),end1) =
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

let pair_to_points (_,(_,_),end0) (start1,(_,_),_) =
  end0,start1

(* export *)

let to_path ~point_radius =
  fold (Point.to_path ~radius:point_radius) Point.pair_to_path

let to_box =
  binop Point.to_box Gg.Box3.union

let pp fmt (point0,(point,_),point1) =
  Format.fprintf fmt "[%a ; %a ; %a]"
    Point.pp point0
    Point.pp (Point.v3_of_v4 point)
    Point.pp point1

let pp_full fmt cluster =
  let f point () = Format.fprintf fmt "%a" Point.pp point
  and g _ _ () = () in
  fold f g cluster ()

(* misc *)

(*
let total_length css =
  let f _ sum = sum
  and g p0 p1 sum = sum +. Gg.V2.(norm (p0 - p1)) in
  NEList.fold f g
    (NEList.flatten @@ NEList.map (fun (c,_) -> to_points c) css)
    0.
 *)

let size (_,(point,_),_) = Gg.V4.w point

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
  Point.middle end0 start2
