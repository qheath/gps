module Pos (*: sig
  type t

  val of_lexbuf : Lexing.lexbuf -> unit -> t
  val of_token : int -> t
  val pp : Format.formatter -> t -> unit
end*) = struct
  type t = Lexing.position * Lexing.position

  let of_lexbuf lexbuf =
    let start = lexbuf.Lexing.lex_start_p in
    fun () -> start,lexbuf.Lexing.lex_curr_p

  let pp fmt (start,curr) =
    let pos_to_string (start,curr) =
      let l1 = start.Lexing.pos_lnum in
      let l2 = curr.Lexing.pos_lnum in
      let c1 = start.Lexing.pos_cnum - start.Lexing.pos_bol + 1 in
      let c2 = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      if l1 < l2 then
        Printf.sprintf "line %d, byte %d - line %d, byte %d" l1 c1 l2 c2
      else if c1 < c2  then
        Printf.sprintf "line %d, bytes %d-%d" l2 c1 c2
      else
        Printf.sprintf "line %d, byte %d" l2 c2
    in
    let name = curr.Lexing.pos_fname in
    if name = "" then
      (* lexbuf line information is rarely accurate at the toplevel,
       * but character information still is! *)
      Format.fprintf fmt "At %s" (pos_to_string (start,curr))
    else
      Format.fprintf fmt "In file %S, at %s" name (pos_to_string (start,curr))
end
