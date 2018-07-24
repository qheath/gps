module type TALKER = sig
  val id : string
  type t
  val pp : Format.formatter -> t -> unit
  val to_point : start:Ptime.t -> t -> Gg.V3.t option
end

module Make (T : TALKER) : sig

  type t =
    | Talker of T.t
    | Proprietary
    | Query

  (** Output a generic line of text (with a proper EOL). *)
  val fprintlf :
    Format.formatter -> ('a, Format.formatter, unit) format -> 'a

  (** Output a sentence. *)
  val pp : Format.formatter -> t -> unit

  val to_point : start:Ptime.t -> t -> Gg.V3.t option

end
