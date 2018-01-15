(** OCaml Interface for Numpy. *)

(** Arrays are passed in place (without copy): Python and OCaml programs
    can change the contents of the array and the changes are visible in
    the other language. *)

(** The following table gives the correspondence between bigarray kinds
    and Numpy element types.
 {ul
 {li [float32] / [NPY_FLOAT]}
 {li [float64] / [NPY_DOUBLE]}
 {li [int8_signed] / [NPY_BYTE]}
 {li [int8_unsigned] / [NPY_UBYTE]}
 {li [int16_signed] / [NPY_SHORT]}
 {li [int16_unsigned] / [NPY_USHORT]}
 {li [int32] / [NPY_INT]}
 {li [int64] / [NPY_LONGLONG]}
 {li [nativeint] / [NPY_LONG]}
 {li [complex32] / [NPY_CFLOAT]}
 {li [complex64] / [NPY_CDOUBLE]}
 {li [char] / [NPY_CHAR]}}
 Other kinds/element types are not supported. In particular, OCaml
 integer kind, [int], has no equivalent type in Numpy. *)

val of_bigarray:
  ('a, 'b, 'c) Bigarray.Genarray.t -> Py.Object.t
(** [of_bigarray a] returns a Numpy array that shares the same contents
    than the OCaml array [a]. *)

val to_bigarray:
  ('a, 'b) Bigarray.kind -> 'c Bigarray.layout -> Py.Object.t ->
    ('a, 'b, 'c) Bigarray.Genarray.t
(** [to_bigarray kind layout a] returns a bigarray that shares the same
    contents than the Numpy array [a]. *)
