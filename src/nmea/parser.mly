%token <(int * int * int)> DATE
%token <(int * int * float)> TIME
%token <Coordinates.Coordinate.t> COORD

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
%type <GP.segment NEList.t> sony_gps_file


%%

/*
file:
  | segments EOF                { List.rev $1 }

segments:
  | segments segment            { $2::$1 }
  |                             { [] }
 */

sony_gps_file:
  | h = sony_gps_segment ; t = list(sony_gps_segment) EOF
                                { NEList.(push h (of_list t)) }

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
  | SENTENCE_PREFIX talker_sentence EOL         { GP.Talker $2 }
/*
  | SENTENCE_PREFIX proprietary_sentence EOL    { $3 }
  | SENTENCE_PREFIX query_sentence EOL          { $3 }
 */

talker_sentence:
  | GP GGA COMMA gp_gga_sentence        { GP.Talker.GGA $4 }
  | GP RMC COMMA gp_rmc_sentence        { GP.Talker.RMC $4 }

gp_gga_sentence:
  | time COMMA coords COMMA NAT COMMA NAT COMMA COMMA COMMA UNIT COMMA COMMA UNIT COMMA COMMA STAR checksum
        { GP.Talker.({
            ptime = $1 ;
            coordinates = $3 ;
            gps_quality = $5 ;
            satellites_number = $7 ;
            dilution = None ;
            antenna_altitude = None ;
            separation = None ;
            age = None ;
            station_id = 0 ;
            checksum = $18 ;
        })}

gp_rmc_sentence:
  | TIME COMMA STATUS COMMA coords COMMA REAL COMMA COMMA DATE COMMA COMMA COMMA STATUS STAR checksum
        { GP.Talker.({
            ptime = Utils.utc_of_dt ($10,$1) ;
            coordinates = $5 ;
            speed = $7 ;
            track = None ;
            variation = None ;
            checksum = $16 ;
        })}

date_time:
  | DATE TIME                   { Utils.utc_of_dt ($1,$2) }

time:
  | TIME                        { Utils.utc_of_t $1 }

checksum:
  | HEX                         { $1 }
  | NAT                         { int_of_string (Printf.sprintf "0x%d" $1) }

coords:
  | COORD COMMA NS COMMA COORD COMMA EW { ($1,$3),($5,$7) }
