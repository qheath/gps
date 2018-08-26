let utc_of_dt =
  let get = function None -> assert false | Some v -> v in
  fun (d,(h,m,ss)) ->
    let s = floor ss in
    let ps = Int64.of_float ((ss-.s) *. 1e+12) in
    get @@ Ptime.add_span
             (get @@ Ptime.of_date_time (d, ((h,m,int_of_float s), 0)))
             (get @@ Ptime.Span.of_d_ps (0,ps))

let utc_of_t =
  let get = function None -> assert false | Some v -> v in
  fun (h,m,ss) ->
    let s = floor ss in
    let ps = Int64.of_float ((ss-.s) *. 1e+12) in
    get @@ Ptime.add_span
             (get @@ Ptime.of_date_time (Ptime.to_date Ptime.epoch, ((h,m,int_of_float s), 0)))
             (get @@ Ptime.Span.of_d_ps (0,ps))

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
