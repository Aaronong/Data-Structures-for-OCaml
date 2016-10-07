(* Interface for int-only UF *)
type uf_t

val make -> uf_t

exception AlreadyThere (* if we are already in the structure *)
exception NotValid (* if we are out of bounds *)
val singleton : uf_t -> int -> unit

(* This combines make and singleton for each element. *)
val make_full -> uf_t

exception NotFound
val find : uf_t -> int -> int
val union : uf_t -> int -> int -> unit

(* For comparison purposes *)
val find_nocompress : uf_t -> int -> int

val union_norank : uf_t -> int -> int -> unit
val union_nocompress : uf_t -> int -> int -> unit
val union_norank_nocompress : uf_t -> int -> int -> unit

(* Return number of components *)
val count : uf_t -> int

