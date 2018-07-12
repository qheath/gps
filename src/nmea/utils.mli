(** UTC time from date and time. *)
val utc_of_dt : (Ptime.date * (int * int * float)) -> Ptime.t

(** UTC time from time (date is supposed to be epoch). *)
val utc_of_t : (int * int * float) -> Ptime.t

(** DDMMYY *)
val pp_dmy : Format.formatter -> Ptime.t -> unit

(** YYYYMMDD *)
val pp_date : Format.formatter -> Ptime.t -> unit

(** HHMMSS.SS *)
val pp_time : Format.formatter -> Ptime.t -> unit

(** Checksum of characters. *)
val checksum : ?seed:int -> string -> int
