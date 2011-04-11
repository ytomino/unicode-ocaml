type ucs4 = Int32.t
module UCS4: sig
	include module type of Int32 with type t = ucs4
	val of_int: int -> t
end
type utf8_char = char
type utf8_string = string
type utf16_char = int
type utf16_string = (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type utf32_char = UCS4.t
type utf32_string = (UCS4.t, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
val utf8_sequence: ?invalid_sequence:exn -> utf8_char -> int
val utf8_get_code: ?invalid_sequence:exn -> utf8_string -> int ref -> int
val utf8_set_code: ?invalid_sequence:exn -> utf8_string -> int ref -> int -> unit
val utf8_lead: utf8_string -> int -> int
val utf16_sequence: ?invalid_sequence:exn -> utf16_char -> int
val utf16_get_code: ?invalid_sequence:exn -> utf16_string -> int ref -> int
val utf16_set_code: ?invalid_sequence:exn -> utf16_string -> int ref -> int -> unit
val utf16_lead: utf16_string -> int -> int
val utf32_sequence: ?invalid_sequence:exn -> utf32_char -> int
val utf32_get_code: ?invalid_sequence:exn -> utf32_string -> int ref -> int
val utf32_set_code: ?invalid_sequence:exn -> utf32_string -> int ref -> int -> unit
val utf32_lead: utf32_string -> int -> int
val utf8_of_utf16: ?invalid_sequence:exn -> utf16_string -> utf8_string
val utf8_of_utf32: ?invalid_sequence:exn -> utf32_string -> utf8_string
val utf16_of_utf8: ?invalid_sequence:exn -> utf8_string -> utf16_string
val utf16_of_utf32: ?invalid_sequence:exn -> utf32_string -> utf16_string
val utf32_of_utf8: ?invalid_sequence:exn -> utf8_string -> utf32_string
val utf32_of_utf16: ?invalid_sequence:exn -> utf16_string -> utf32_string
module UTF8: sig
	type elm = utf8_char
	type t = utf8_string
	val compare: t -> t -> int
	external length: t -> int = "%string_length"
	external get: t -> int -> char = "%string_safe_get"
	external unsafe_get: t -> int -> char = "%string_unsafe_get"
	external set: t -> int -> char -> unit = "%string_safe_set"
	external unsafe_set: t -> int -> char -> unit = "%string_unsafe_set"
	val empty: t
	external create: int -> t = "caml_create_string"
	val make: int -> char -> t
	val copy: t -> t
	val append: t -> t -> t
	val sub: t -> int -> int -> t
	val fill: t -> int -> int -> char -> unit
	external unsafe_fill: t -> int -> int -> char -> unit = "caml_fill_string" "noalloc"
	val blit: t -> int -> t -> int -> int -> unit
	external unsafe_blit: t -> int -> t -> int -> int -> unit = "caml_blit_string" "noalloc"
	val concat: t -> t list -> t
	val iter: (char -> unit) -> t -> unit
	val escaped: t -> t
	val index: t -> char -> int
	val rindex: t -> char -> int
	val index_from: t -> int -> char -> int
	val rindex_from: t -> int -> char -> int
	val contains: t -> char -> bool
	val contains_from: t -> int -> char -> bool
	val rcontains_from: t -> int -> char -> bool
	val sequence: ?invalid_sequence:exn -> elm -> int
	val max_sequence: int
	val get_code: ?invalid_sequence:exn -> t -> int ref -> int
	val set_code: ?invalid_sequence:exn -> t -> int ref -> int -> unit
	val lead: t -> int -> int
	val of_utf16: ?invalid_sequence:exn -> utf16_string -> t
	val of_utf32: ?invalid_sequence:exn -> utf32_string -> t
	val of_array: elm array -> t
end
module UTF16: sig
	type elm = utf16_char
	type t = utf16_string
	external compare: t -> t -> int = "%compare"
	val length: t -> int
	external get: t -> int -> elm = "%caml_ba_ref_1"
	external unsafe_get: t -> int -> elm = "%caml_ba_unsafe_ref_1"
	external set: t -> int -> elm -> unit = "%caml_ba_set_1"
	external unsafe_set: t -> int -> elm -> unit = "%caml_ba_unsafe_set_1"
	val empty: t
	val create: int -> t
	val copy: t -> t
	val append: t -> t -> t
	val sub: t -> int -> int -> t
	external fill: t -> elm -> unit = "caml_ba_fill"
	external blit: t -> t -> unit = "caml_ba_blit"
	val sequence: ?invalid_sequence:exn -> elm -> int
	val max_sequence: int
	val get_code: ?invalid_sequence:exn -> t -> int ref -> int
	val set_code: ?invalid_sequence:exn -> t -> int ref -> int -> unit
	val lead: t -> int -> int
	val of_utf8: ?invalid_sequence:exn -> utf8_string -> t
	val of_utf32: ?invalid_sequence:exn -> utf32_string -> t
	val of_array: elm array -> t
end
module UTF32: sig
	type elm = utf32_char
	type t = utf32_string
	external compare: t -> t -> int = "%compare"
	val length: t -> int
	external get: t -> int -> elm = "%caml_ba_ref_1"
	external unsafe_get: t -> int -> elm = "%caml_ba_unsafe_ref_1"
	external set: t -> int -> elm -> unit = "%caml_ba_set_1"
	external unsafe_set: t -> int -> elm -> unit = "%caml_ba_unsafe_set_1"
	val empty: t
	val create: int -> t
	val copy: t -> t
	val append: t -> t -> t
	val sub: t -> int -> int -> t
	external fill: t -> elm -> unit = "caml_ba_fill"
	external blit: t -> t -> unit = "caml_ba_blit"
	val sequence: ?invalid_sequence:exn -> elm -> int
	val max_sequence: int
	val get_code: ?invalid_sequence:exn -> t -> int ref -> int
	val set_code: ?invalid_sequence:exn -> t -> int ref -> int -> unit
	val lead: t -> int -> int
	val of_utf8: ?invalid_sequence:exn -> utf8_string -> t
	val of_utf16: ?invalid_sequence:exn -> utf16_string -> t
	val of_array: elm array -> t
end
