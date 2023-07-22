module Interp = Parser.MenhirInterpreter
open Lwt.Syntax

type answer =
  | Yes of GP.segment NEList.t
  | Parser of int * (Lexing.position * Lexing.position)
  | Lexer of string

let parse_sony_gps_file lexbuf =
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
    | Yes segments -> Lwt.return @@ Some segments
    | Parser (state,position) ->
      let* () =
        match
          (*try Some (Parser_messages.message (Interp.number state))
            with Not_found ->*) None
        with
        | None ->
          let tags =
            Logs.Tag.(empty |> add JupiterI.Pos.tag_def (JupiterI.Pos.of_positions position))
          in
          JupiterI.Output.err
            (fun m -> m ~tags "parser state %d reached, cannot go forward@." state)
        | Some message ->
          let tags =
            Logs.Tag.(empty |> add JupiterI.Pos.tag_def (JupiterI.Pos.of_positions position))
          in
          JupiterI.Output.err
            (fun m -> m ~tags "%s@." message)
      in
      Lwt.return None
    | Lexer message ->
      let tags =
        Logs.Tag.(empty |> add JupiterI.Pos.tag_def (JupiterI.Pos.of_lexbuf lexbuf ()))
      in
      let* () = JupiterI.Output.err (fun m -> m ~tags "lexing error: %S@." message) in
      Lwt.return @@ None
  in
  close_in ic ;
  segments
