(** {6 Low-level access} *)

type answer =
  (** Input success. *)
  | Yes of GP.segment NEList.t

  (** Parsing error (current state, and position of the last lexeme). *)
  | Parser of int * (Lexing.position * Lexing.position)

  (** Lexing error (internal lexer error message). *)
  | Lexer of string

(** Parse some Sony GPS file segments. *)
val parse_sony_gps_file : Lexing.lexbuf -> answer


(** {6 High-level access} *)

(** Read a Sony GPS file. *)
val read_sony_gps_file : string -> GP.segment NEList.t option
