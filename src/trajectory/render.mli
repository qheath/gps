val render_atoms :
  Atom.t NEList.t ->
  (float * Gg.V2.t * float * Atom.switch) NEList.t option ->
  filename:string ->
  max_outer_pixel_size:Gg.Size2.t ->
  pixel_margin:float ->
  pixel_stroke_width:float ->
  pixel_point_radius:float ->
  unit
