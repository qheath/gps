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

let read_sony_gps_file path =
  let ic = open_in path in
  let segments =
    let lexbuf = Lexing.from_channel ic in
    match parse_sony_gps_file lexbuf with
    | Yes segments -> Some segments
    | Parser (state,position) ->
      begin
        match
          (*try Some (Parser_messages.message (Interp.number state))
            with Not_found ->*) None
        with
        | None ->
          JupiterI.Output.eprintf
            "%a: parser state %d reached, cannot go forward@."
            JupiterI.Pos.pp (JupiterI.Pos.of_positions position) state
        | Some message ->
          JupiterI.Output.eprintf "%a: %s@."
            JupiterI.Pos.pp (JupiterI.Pos.of_positions position) message
      end ;
      None
    | Lexer message ->
      Format.eprintf "%a: lexing error: %S@."
        JupiterI.Pos.pp (JupiterI.Pos.of_lexbuf lexbuf ()) message ;
      None
  in
  close_in ic ;
  segments
