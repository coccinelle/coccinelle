type annot_key =
    Exclude_start
  | Exclude_end

type annot_val =
  Unit

type annots

val empty : annots
val get_annot : annots -> annot_key -> annot_val option
val put_annot : annot_key -> annot_val -> annots -> annots
