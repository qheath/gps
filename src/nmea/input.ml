module Interp = Parser.MenhirInterpreter

type answer =
  | Yes of GP.segment NEList.t
  | Parser of int * (Lexing.position * Lexing.position)
  | Lexer of string

let parse_sony_gps_file lexbuf =
  let open MenhirLib.General in
  let input = Interp.lexer_lexbuf_to_supplier Lexer.token lexbuf
  and success x = Yes x
  and failure = function
    | Interp.HandlingError env ->
        begin match Interp.top env with
          | Some (Interp.Element (state,_,start_pos,end_pos)) ->
              Parser (Interp.number state,(start_pos,end_pos))
          | None -> assert false
        end
    | _ -> assert false
  in
  try
    Interp.loop_handle success failure input
      (Parser.Incremental.sony_gps_file lexbuf.Lexing.lex_curr_p)
  with e -> Lexer (Printexc.to_string e)
