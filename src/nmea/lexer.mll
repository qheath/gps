{
  open Parser
  open Lexing
}

let digit = ['0'-'9']
let date = digit digit digit digit digit digit digit digit
let time = digit? digit digit digit digit digit '.' digit+
let coord = ((digit? digit)? digit)? digit digit '.' digit+
let real = digit? digit '.' digit+
let dmy = digit digit digit digit digit digit?
let nat = digit (digit (digit digit?)?)?

let hexit = digit | ['A'-'F']
let hex = hexit hexit

let hours = digit digit
let minutes = digit digit
let seconds = digit digit
let miliseconds = digit digit digit

rule token = parse
  | dmy                         { let date = int_of_string (Lexing.lexeme lexbuf) in
                                  let day = date/10000 in
                                  let month = date/100 - 100*day in
                                  let year = date - 100*month - 10000*day in
                                  DATE ((if year<70 then year+2000 else year+1900),month,day) }
  | date                        { let date = int_of_string (Lexing.lexeme lexbuf) in
                                  let year = date/10000 in
                                  let month = date/100 - 100*year in
                                  let day = date - 100*month - 10000*year in
                                  DATE (year,month,day) }
  | time                        { let time = float_of_string (Lexing.lexeme lexbuf) in
                                  let hours = (int_of_float time)/10000 in
                                  let minutes = (int_of_float time)/100 - 100*hours in
                                  let seconds = time -. 100.*.((float)(minutes + 100*hours)) in
                                  TIME (hours,minutes,seconds) }
  | coord                       { let coord = float_of_string (Lexing.lexeme lexbuf) in
                                  let degrees = (int_of_float coord)/100 in
                                  let minutes = coord -. ((float)(100 * degrees)) in
                                  COORD (Coordinates.Coordinate.DDM (degrees,minutes)) }
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
