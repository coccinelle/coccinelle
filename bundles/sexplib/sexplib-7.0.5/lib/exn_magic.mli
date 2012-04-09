(******************************************************************************
 *                             Sexplib                                        *
 *                                                                            *
 * Copyright (C) 2005- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *    Author: Markus Mottl                                                    *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

val register : exn -> string -> unit

val register1 :
  ('a -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> unit

val register2 :
  ('a -> 'b -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> unit

val register3 :
  ('a -> 'b -> 'c -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> unit

val register4 :
  ('a -> 'b -> 'c -> 'd -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> unit

val register5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> unit

val register6 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> ('f -> Sexp.t)
  -> unit

val register7 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> ('f -> Sexp.t)
  -> ('g -> Sexp.t)
  -> unit

val register8 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> ('f -> Sexp.t)
  -> ('g -> Sexp.t)
  -> ('h -> Sexp.t)
  -> unit

val register9 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> ('f -> Sexp.t)
  -> ('g -> Sexp.t)
  -> ('h -> Sexp.t)
  -> ('i -> Sexp.t)
  -> unit

val register10 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> exn)
  -> string
  -> ('a -> Sexp.t)
  -> ('b -> Sexp.t)
  -> ('c -> Sexp.t)
  -> ('d -> Sexp.t)
  -> ('e -> Sexp.t)
  -> ('f -> Sexp.t)
  -> ('g -> Sexp.t)
  -> ('h -> Sexp.t)
  -> ('i -> Sexp.t)
  -> ('j -> Sexp.t)
  -> unit
