(*
let () = Random.self_init ()
 *)

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

let pp fmt (cluster,switch) =
  Format.fprintf fmt "[%s]%a"
    (match switch with Left -> "Left" | Right -> "Right")
    Cluster.pp cluster

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
