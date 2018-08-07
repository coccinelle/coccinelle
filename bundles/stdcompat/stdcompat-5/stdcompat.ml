include Stdcompat__root

module Stdlib = struct
  module Pervasives = Stdcompat__pervasives

  include Pervasives

  module Arg = Stdcompat__arg

  module Array = Stdcompat__array

  module ArrayLabels = Stdcompat__arrayLabels

(* (* Bigarray is not exported to allow programs not
  to be compiled with this module, this may change in the future. *)
  module Bigarray = Bigarray
*)

  module Buffer = Stdcompat__buffer

  module Bytes = Stdcompat__bytes

  module BytesLabels = Stdcompat__bytesLabels

  module Callback = Callback

  module Char = Stdcompat__char

  module Complex = Complex

  module Digest = Stdcompat__digest

  module Ephemeron = Stdcompat__ephemeron

  module Filename = Stdcompat__filename

  module Float = Stdcompat__float

  module Format = Format

  module Gc = Gc

  module Genlex = Genlex

  module Hashtbl = Stdcompat__hashtbl

  module Int32 = Stdcompat__int32

  module Int64 = Stdcompat__int64

  module Lazy = Stdcompat__lazy

  module Lexing = Lexing

  module List = Stdcompat__list

  module ListLabels = Stdcompat__listLabels

  module Map = Stdcompat__map

  module Marshal = Marshal

  module MoreLabels = MoreLabels

  module Nativeint = Stdcompat__nativeint

  module Obj = Obj

  module Oo = Oo

  module Parsing = Parsing

  module Printexc = Printexc

  module Printf = Printf

  module Queue = Stdcompat__queue

  module Random = Random

  module Scanf = Scanf

  module Seq = Stdcompat__seq

  module Set = Stdcompat__set

  module Sort = Sort

  module Spacetime = Stdcompat__spacetime

  module Stack = Stdcompat__stack

  module StdLabels = struct
    module Array = Stdcompat__arrayLabels

    module Bytes = Stdcompat__bytesLabels

    module List = Stdcompat__listLabels

    module String = Stdcompat__stringLabels
  end

  module Stream = Stdcompat__stream

  module String = Stdcompat__string

  module StringLabels = Stdcompat__stringLabels

  module Sys = Stdcompat__sys

  module Uchar = Stdcompat__uchar

  module Weak = Stdcompat__weak
end

include Stdlib
