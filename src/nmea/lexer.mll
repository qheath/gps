{
  open Parser
  open Lexing
}

let digit = ['0'-'9']
let real = ((((digit? digit)? digit)? digit)? digit)? digit '.' digit+
let nat = ((((((digit? digit)? digit)? digit)? digit)? digit)? digit)? digit

let hexit = digit | ['A'-'F']
let hex = hexit hexit

rule token = parse
  | nat                         { NAT (int_of_string (Lexing.lexeme lexbuf)) }
  | real                        { REAL (float_of_string (Lexing.lexeme lexbuf)) }
  | hex                         { HEX (int_of_string ("0x"^(Lexing.lexeme lexbuf))) }
  | '*'                         { STAR }
  | 'N'                         { NS `N }
  | 'S'                         { NS `S }
  | 'E'                         { EW `E }
  | 'W'                         { EW `W }
  | 'M'                         { UNIT (Lexing.lexeme lexbuf) }
  | 'A'                         { STATUS true }
  | 'V'                         { STATUS false }
  | 'D'                         { STATUS true }

  | "@Sonygps/ver"              { SONY_VERSION }
  | "wgs-84"                    { WGS84 }
  | "@Sonygpsoption/0"          { SONY_OPTIONS }

  | '$'                         { SENTENCE_PREFIX }
  | "GP"                        { GP }
  | "GGA"                       { GGA }
  | "RMC"                       { RMC }

  | '/'                         { SLASH }
  | ','                         { COMMA }
  | "\r\n" | '\n'               { new_line lexbuf ; EOL }

  | eof                         { EOF }

  | _ as c                      { failwith (Format.sprintf "invalid string starting with %C" c) }
