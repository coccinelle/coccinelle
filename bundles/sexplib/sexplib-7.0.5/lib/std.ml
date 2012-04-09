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

module Hashtbl = struct
  include Hashtbl

  let sexp_of_t = Conv.sexp_of_hashtbl
  let t_of_sexp = Conv.hashtbl_of_sexp
end

module Big_int = struct
  include Big_int

  let sexp_of_big_int = Conv.sexp_of_big_int
  let big_int_of_sexp = Conv.big_int_of_sexp
end

module Nat = struct
  include Nat

  let sexp_of_nat = Conv.sexp_of_nat
  let nat_of_sexp = Conv.nat_of_sexp
end

module Num = struct
  include Num

  let sexp_of_num = Conv.sexp_of_num
  let num_of_sexp = Conv.num_of_sexp
end

module Ratio = struct
  include Ratio

  let sexp_of_ratio = Conv.sexp_of_ratio
  let ratio_of_sexp = Conv.ratio_of_sexp
end

module Lazy = struct
  include Lazy

  let t_of_sexp = Conv.lazy_t_of_sexp
  let sexp_of_t = Conv.sexp_of_lazy_t
end

let sexp_of_unit = Conv.sexp_of_unit
let unit_of_sexp = Conv.unit_of_sexp

let bool_of_sexp = Conv.bool_of_sexp
let sexp_of_bool = Conv.sexp_of_bool

let string_of_sexp = Conv.string_of_sexp
let sexp_of_string = Conv.sexp_of_string

let char_of_sexp = Conv.char_of_sexp
let sexp_of_char = Conv.sexp_of_char

let int_of_sexp = Conv.int_of_sexp
let sexp_of_int = Conv.sexp_of_int

let float_of_sexp = Conv.float_of_sexp
let sexp_of_float = Conv.sexp_of_float

let int32_of_sexp = Conv.int32_of_sexp
let sexp_of_int32 = Conv.sexp_of_int32

let int64_of_sexp = Conv.int64_of_sexp
let sexp_of_int64 = Conv.sexp_of_int64

let nativeint_of_sexp = Conv.nativeint_of_sexp
let sexp_of_nativeint = Conv.sexp_of_nativeint

let sexp_of_ref = Conv.sexp_of_ref
let ref_of_sexp = Conv.ref_of_sexp

let sexp_of_lazy_t = Conv.sexp_of_lazy_t
let lazy_t_of_sexp = Conv.lazy_t_of_sexp

let option_of_sexp = Conv.option_of_sexp
let sexp_of_option = Conv.sexp_of_option

let list_of_sexp = Conv.list_of_sexp
let sexp_of_list = Conv.sexp_of_list

let array_of_sexp = Conv.array_of_sexp
let sexp_of_array = Conv.sexp_of_array

let sexp_of_exn = Conv.sexp_of_exn
