module type TALKER = sig
  val id : string
  type t
  val pp : Format.formatter -> t -> unit
  val to_point : start:Ptime.t -> t -> Gg.V3.t option
end

module Make (T : TALKER) = struct

  type t =
    | Talker of T.t
    | Proprietary
    | Query

  let fprintlf chan fmt =
    Format.fprintf chan (fmt ^^ "\r\n%!")

  let pp chan =
    let aux id f = fprintlf chan "$%s%a" id f in
    function
      | Talker talker -> aux T.id T.pp talker
      | Proprietary -> aux "" (fun chan () -> ()) ()
      | Query -> aux "" (fun chan () -> ()) ()

  let to_point ~start = function
    | Talker talker_sentence -> T.to_point ~start talker_sentence
    | Proprietary -> None
    | Query -> None

end
