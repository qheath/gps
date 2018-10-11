let utc_of_time =
  let get = function None -> assert false | Some v -> v in
  fun ?(date=Ptime.to_date Ptime.epoch) time ->
    let hours,minutes,(seconds,pico_seconds) =
      let hours = (int_of_float time)/10000 in
      let minutes = (int_of_float time)/100 - 100*hours in
      let decimal_seconds = time -. 100.*.((float)(minutes + 100*hours)) in
      let seconds = floor decimal_seconds in
      let pico_seconds = Int64.of_float ((decimal_seconds-.seconds) *. 1e+12) in
      hours,minutes,(int_of_float seconds,pico_seconds)
    in
    get @@ Ptime.add_span
             (get @@ Ptime.of_date_time (date, ((hours,minutes,seconds), 0)))
             (get @@ Ptime.Span.of_d_ps (0,pico_seconds))

let pp_dmy chan ptime =
  let y,m,d = Ptime.to_date ptime in
  Format.fprintf chan "%02d%02d%02d" d m (y mod 100)

let pp_date chan ptime =
  let y,m,d = Ptime.to_date ptime in
  Format.fprintf chan "%04d%02d%02d" y m d

let pp_time chan ptime =
  let _,((h,m,s),_) = Ptime.to_date_time ptime
  and frac = Ptime.Span.to_float_s @@ Ptime.frac_s ptime in
  Format.fprintf chan "%.2d%.2d%06.3f" h m ((float)s +. frac)

let pp_datetime chan ptime =
  Format.fprintf chan "%a%a" pp_date ptime pp_time ptime

let checksum =
  let string_fold f seed s =
    let explode s =
      let rec aux accum i =
        if i<0 then accum else aux (String.get s i::accum) (i-1)
      in
      aux [] (String.length s)
    in
    List.fold_left f seed (explode s)
  in
  let f x c = x lxor (Char.code c) in
  fun ?(seed=0) s -> string_fold f seed s
