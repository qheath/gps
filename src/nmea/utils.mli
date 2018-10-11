(** UTC time from date (defaults to epoch) and time. *)
val utc_of_time : ?date:Ptime.date -> float -> Ptime.t

(** DDMMYY *)
val pp_dmy : Format.formatter -> Ptime.t -> unit

(** YYYYMMDD *)
val pp_date : Format.formatter -> Ptime.t -> unit

(** HHMMSS.SS *)
val pp_time : Format.formatter -> Ptime.t -> unit

(** YYYYMMDDHHMMSS.SS *)
val pp_datetime : Format.formatter -> Ptime.t -> unit

(** Checksum of characters. *)
val checksum : ?seed:int -> string -> int
