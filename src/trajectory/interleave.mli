val process : odir:string -> ?nb_iters:int -> Atom.t NEList.t ->
  Atom.t NEList.t * (float * Gg.V2.t * float * Atom.switch) NEList.t option
