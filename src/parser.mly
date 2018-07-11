%token <(int * int * int)> DATE
%token <(int * int * float)> TIME
%token <Types.Coordinates.coordinate> COORD

%token <int> NAT
%token <float> REAL
%token <int> HEX

%token <[ `N | `S ]> NS
%token <[ `E | `W ]> EW
%token <string> UNIT
%token <bool> STATUS

%token SONY_VERSION WGS84 SONY_OPTIONS SENTENCE_PREFIX GP GGA RMC
%token SLASH COMMA STAR
%token EOL EOF

%start sony_gps_file
%type <((Ptime.t * (Ptime.t * Ptime.t)) * Types.GP.sentence list) list> sony_gps_file


%%

/*
file:
  | segments EOF                { List.rev $1 }

segments:
  | segments segment            { $2::$1 }
  |                             { [] }
 */

sony_gps_file:
  | l = list(sony_gps_segment) EOF
                                { l }

sony_gps_segment:
  | sony_header sentences       { $1,$2 }

sony_header:
  | sony_version EOL sony_options EOL
                                { $1,$3 }

sony_version:
  | SONY_VERSION REAL SLASH WGS84 SLASH date_time SLASH
                                { (*$2,*)$6 }

sony_options:
  | SONY_OPTIONS SLASH date_time SLASH date_time SLASH
                                { $3,$5 }

sentences:
  | ss = list(sentence)         { ss }

sentence:
  | SENTENCE_PREFIX talker_sentence EOL         { Types.GP.Talker $2 }
/*
  | SENTENCE_PREFIX proprietary_sentence EOL    { $3 }
  | SENTENCE_PREFIX query_sentence EOL          { $3 }
 */

talker_sentence:
  | GP GGA COMMA gp_gga_sentence        { Types.GP.GGA $4 }
  | GP RMC COMMA gp_rmc_sentence        { Types.GP.RMC $4 }

gp_gga_sentence:
  | time COMMA coords COMMA NAT COMMA NAT COMMA COMMA COMMA UNIT COMMA COMMA UNIT COMMA COMMA STAR checksum
        { $1,$3,$5,$7,None,None,None,None,0,$18 }

gp_rmc_sentence:
  | TIME COMMA STATUS COMMA coords COMMA REAL COMMA COMMA DATE COMMA COMMA COMMA STATUS STAR checksum
        { Types.utc_of_dt ($10,$1),$5,$7,None,None,$16 }

date_time:
  | DATE TIME                   { Types.utc_of_dt ($1,$2) }

time:
  | TIME                        { Types.utc_of_t $1 }

checksum:
  | HEX                         { $1 }
  | NAT                         { int_of_string (Printf.sprintf "0x%d" $1) }

coords:
  | COORD COMMA NS COMMA COORD COMMA EW { ($1,$3),($5,$7) }
