type answer =
  (** Input success. *)
  | Yes of GP.segment list

  (** Parsing error (current state, and position of the last lexeme). *)
  | Parser of int * (Lexing.position * Lexing.position)

  (** Lexing error (internal lexer error message). *)
  | Lexer of string

(** Parse some Sony GPS files. *)
val parse_sony_gps_file : Lexing.lexbuf -> answer
