module type TALKER = sig
  val id : string
  type t
  val pp : Format.formatter -> t -> unit
  val to_point : start:Ptime.t -> t -> Gg.V3.t option
  val of_point : start:Ptime.t -> Gg.V3.t -> t option
end

module Make (T : TALKER) = struct

  type t =
    | Talker of T.t
    | Proprietary
    | Query

  let fprintlf fmt f =
    Format.fprintf fmt (f ^^ "\r\n%!")

  let pp fmt =
    let aux id f = fprintlf fmt "$%s%a" id f in
    function
      | Talker talker -> aux T.id T.pp talker
      | Proprietary -> aux "" (fun _fmt () -> ()) ()
      | Query -> aux "" (fun _fmt () -> ()) ()

  let to_point ~start = function
    | Talker talker_sentence -> T.to_point ~start talker_sentence
    | Proprietary -> None
    | Query -> None

  let of_point ~start point =
    match T.of_point ~start point with
    | None -> None
    | Some talker_sentence -> Some (Talker talker_sentence)

end
