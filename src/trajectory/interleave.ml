open Lwt.Syntax

module Cursor : sig

  type 'a t1 = 'a NEList.t option * 'a * 'a NEList.t option
  type 'a t2 = 'a NEList.t option * 'a * 'a * 'a NEList.t option

  val make1 : 'a NEList.t -> 'a t1

  val rewind : 'a t1 -> 'a NEList.t

  val go_forward : 'a t1 -> 'a t1 option

end = struct

  type 'a t1 = 'a NEList.t option * 'a * 'a NEList.t option
  type 'a t2 = 'a NEList.t option * 'a * 'a * 'a NEList.t option

  let make1 elements =
    let element,elements' = NEList.pop elements in
    None,element,elements'

  let rewind (backward',element,forward') =
    let forward = NEList.push element forward' in
    match backward' with
    | Some backward -> NEList.(rev_append (to_list backward) forward)
    | None -> forward


  let go_forward (backward0',element0,forward0') =
    match forward0' with
    | None -> None
    | Some forward1 ->
      let backward1' = Some (NEList.push element0 backward0')
      and element1,forward1' = NEList.pop forward1 in
      Some (backward1',element1,forward1')

end

type state =
  | Init of Atom.t NEList.t
  | Unsplitable of Atom.t NEList.t
  | SplitableN of (Atom.t Cursor.t1 * float)
  | FoundSplitableClusterN of (Atom.t Cursor.t1 * float)
  | DidSplitClusterN of (Atom.t Cursor.t2 * float)

let test_splitability (atoms,threshold) =
  if threshold<2. then Unsplitable atoms else
    SplitableN (Cursor.make1 atoms,threshold)

let clusterise =
  let group_clusters_by_switch =
    let rec aux backward0' (clusters0,s0) forward0' =
      match forward0' with
      | None -> NEList.push (clusters0,s0) backward0'
      | Some forward1 ->
        let (cluster1,s1),forward1' = NEList.pop forward1 in
        let backward1',clusters1' =
          if s0=s1 then backward0',Some clusters0
          else Some (NEList.push (clusters0,s0) backward0'),None
        in
        aux backward1' ((NEList.push cluster1 clusters1'),s1) forward1'
    in
    fun forward0 ->
      let (cluster0,s0),forward0' = NEList.pop forward0 in
      aux None (NEList.push cluster0 None,s0) forward0'
  in
  fun atoms ->
    let atoms =
      NEList.rev_map
        (fun (clusters,s) -> (Cluster.multi_merge @@ NEList.rev clusters),s)
        (group_clusters_by_switch atoms)
    in
    atoms

let find_splitable_cluster
    (_,(cluster0,_),_ as cursor0)
    threshold
  =
  if Cluster.size cluster0 >= threshold
  then FoundSplitableClusterN (cursor0,threshold)
  else match Cursor.go_forward cursor0 with
    | None -> Init (Cursor.rewind cursor0)
    | Some cursor1 ->
      SplitableN (cursor1,threshold)

let split_cluster (backward0',(cluster,s),forward1') threshold =
  match Cluster.split cluster with
  | None -> assert false
  | Some (cluster0,cluster1) ->
    let atom0 = cluster0,s and atom1 = cluster1,s in
    DidSplitClusterN ((backward0',atom0,atom1,forward1'),threshold)

let bubble_clusters =
  let bubble_cluster atom1 =
    let rec aux (backward0',atom0,forward1' as cursor) =
      match forward1' with
      | None ->
        (* we're already at the end *)
        cursor
      | Some forward2 ->
        (* we might swap with atom2... *)
        let atom2,forward2' = NEList.pop forward2 in
        if snd atom1 = snd atom2 then
          (* ... but atom1 and atom2 have the same origin and cannot be
           * swapped *)
          cursor
        else if
          (* ... unless it feels better not to *)
          let atoms =
            match forward2' with
            | None -> []
            | Some forward3 ->
              let x3,_ = NEList.pop forward3 in
              [x3]
          in
          Atom.better
            NEList.(push atom0 (of_list (atom1::atom2::atoms)))
            NEList.(push atom0 (of_list (atom2::atom1::atoms)))
        then cursor
        else aux (Some NEList.(push atom0 backward0'),atom2,forward2')
    in
    fun (atom0,forward1') ->
      let backward0',atom0,forward1' = aux (None,atom0,forward1') in
      (* remove the old atom0 from the end of backward0' before rewinding *)
      let backward',_ = NEList.(pop_back @@ push atom0 backward0') in
      Cursor.rewind (backward',atom1,forward1')
  in
  fun (backward0',atom0,atom1,forward1') threshold ->
    let atom1,forward1' = NEList.pop @@ bubble_cluster atom1 (atom0,forward1') in
    let backward1' = Some (bubble_cluster atom0 (atom1,backward0')) in
    SplitableN ((backward1',atom1,forward1'),threshold)

let average =
  let aux ((cluster0,_),(cluster1,switch1),(cluster2,_)) =
    let point02 = Cluster.middle cluster0 cluster2
    and point1 = Cluster.to_point cluster1 in
    let t02 = Gg.V3.z point02
    and t1 = Gg.V3.z point1 in
    let point = Point.middle point02 point1 in
    t02,Gg.V2.of_v3 point,t1,switch1
  in
  fun atoms ->
    let f _ pairs = pairs
    and g atom0 atom1 pairs = (atom0,atom1)::pairs in
    match List.rev @@ NEList.fold f g atoms [] with
    | [] -> None (* only one atom *)
    | h::t ->
      let pairs = NEList.(push h (of_list t)) in
      let f _ triples = triples
      and g (atom0,atom1) (_,atom2) triples = (atom0,atom1,atom2)::triples in
      match List.rev @@ NEList.fold f g pairs [] with
      | [] -> None (* only two atoms *)
      | h::t ->
        let triples = NEList.(push h (of_list t)) in
        Some (NEList.map aux triples)

let frame ~odir =
  let i = ref 0 in
  fun atoms average' ->
    incr i ;
    Render.render_atoms atoms average'
      ~filename:(Printf.sprintf "%s/%04d" odir !i)
      ~max_outer_pixel_size:(Gg.Size2.v (1366./.2. -. 4.) (768. -. 4.))
      ~pixel_margin:15.
      ~pixel_stroke_width:2.
      ~pixel_point_radius:5.

(*
 * processing is done in iterations; after each, we can compute a score,
 * and either we stop when it stops improving, or when it improved
 * enough, or after a given number of iterations
 *
 * an iteration has two passes: atomisation and clusterisation
 *
 * atomisation starts by looking for the largest cluster size, then does
 * decrementing steps
 *
 * clusterisation is straightforward
 *
 * an atomisation step splits all things larger than a given threshold
 * (without ensuring that the resulting smaller clusters are themselves
 * not larger than the threshold)
 *)


let process ~odir ?(nb_iters=3) =
  let pause = frame ~odir in

  let sub_aux = function
    | Init atoms ->
      let average' = average atoms in
      let* () = pause atoms average' in
      let threshold = Atom.max_size atoms in
      Lwt.return @@ Error (test_splitability (atoms,threshold))
    | Unsplitable atoms ->
      Lwt.return @@ Ok (clusterise atoms)
    | SplitableN (cursor,threshold) ->
      Lwt.return @@ Error (find_splitable_cluster cursor threshold)
    | FoundSplitableClusterN (cursor,threshold) ->
      Lwt.return @@ Error (split_cluster cursor threshold)
    | DidSplitClusterN (cursor2,threshold) ->
      Lwt.return @@ Error (bubble_clusters cursor2 threshold)
  in

  let rec aux (nb_iters,state) =
    let* blah = sub_aux state in
    match blah with
    | Ok atoms ->
      let nb_iters = nb_iters-1 in
      if nb_iters>0 then
        iterations (nb_iters,atoms)
      else begin
        let average' = average atoms in
        let* () = pause atoms average' in
        Lwt.return @@ (atoms,average')
      end
    | Error intermediate_state -> aux (nb_iters,intermediate_state)
  and iterations (nb_iters,atoms) =
    aux (nb_iters,Init atoms)
  in
  fun atoms -> iterations (nb_iters,atoms)
