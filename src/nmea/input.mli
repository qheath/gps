(** {6 Low-level access} *)

type answer =
  | Yes of GP.segment NEList.t
  (** Input success. *)

  | Parser of int * (Lexing.position * Lexing.position)
  (** Parsing error (current state, and position of the last lexeme). *)

  | Lexer of string
  (** Lexing error (internal lexer error message). *)

(** Parse some Sony GPS file segments. *)
val parse_sony_gps_file : Lexing.lexbuf -> answer


(** {6 High-level access} *)

(** Read a Sony GPS file. *)
val read_sony_gps_file : string -> GP.segment NEList.t option Lwt.t
