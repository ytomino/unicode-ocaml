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
val utf8_sequence: ?illegal_sequence:exn -> utf8_char -> int
val utf8_get_code: ?illegal_sequence:exn -> utf8_string -> int ref -> int
val utf8_set_code: ?illegal_sequence:exn -> bytes -> int ref -> int -> unit [@@ocaml.deprecated]
val utf8_lead: utf8_string -> int -> int
val utf16_sequence: ?illegal_sequence:exn -> utf16_char -> int
val utf16_get_code: ?illegal_sequence:exn -> utf16_string -> int ref -> int
val utf16_set_code: ?illegal_sequence:exn -> utf16_string -> int ref -> int -> unit
val utf16_lead: utf16_string -> int -> int
val utf32_sequence: ?illegal_sequence:exn -> utf32_char -> int
val utf32_get_code: ?illegal_sequence:exn -> utf32_string -> int ref -> int
val utf32_set_code: ?illegal_sequence:exn -> utf32_string -> int ref -> int -> unit
val utf32_lead: utf32_string -> int -> int
val utf8_of_utf16: ?illegal_sequence:exn -> utf16_string -> utf8_string
val utf8_of_utf32: ?illegal_sequence:exn -> utf32_string -> utf8_string
val utf16_of_utf8: ?illegal_sequence:exn -> utf8_string -> utf16_string
val utf16_of_utf32: ?illegal_sequence:exn -> utf32_string -> utf16_string
val utf32_of_utf8: ?illegal_sequence:exn -> utf8_string -> utf32_string
val utf32_of_utf16: ?illegal_sequence:exn -> utf16_string -> utf32_string
module UTF8: sig
	type elm = utf8_char
	type t = utf8_string
	val compare: t -> t -> int
	external length: t -> int = "%string_length"
	external get: t -> int -> char = "%string_safe_get"
	external unsafe_get: t -> int -> char = "%string_unsafe_get"
	external set: bytes -> int -> char -> unit = "%string_safe_set" [@@ocaml.deprecated]
	external unsafe_set: bytes -> int -> char -> unit = "%string_unsafe_set" [@@ocaml.deprecated]
	val empty: t
	external create: int -> bytes = "caml_create_string" [@@ocaml.deprecated]
	val copy: t -> t [@@ocaml.deprecated]
	val append: t -> t -> t
	val sub: t -> int -> int -> t
	val fill: bytes -> int -> int -> char -> unit [@@ocaml.deprecated]
	external unsafe_fill: bytes -> int -> int -> char -> unit = "caml_fill_string" [@@noalloc] [@@ocaml.deprecated]
	val blit: t -> int -> bytes -> int -> int -> unit [@@ocaml.deprecated]
	external unsafe_blit: t -> int -> bytes -> int -> int -> unit = "caml_blit_string" [@@noalloc] [@@ocaml.deprecated]
	val sequence: ?illegal_sequence:exn -> elm -> int
	val max_sequence: int
	val get_code: ?illegal_sequence:exn -> t -> int ref -> int
	val set_code: ?illegal_sequence:exn -> bytes -> int ref -> int -> unit [@@ocaml.deprecated]
	val lead: t -> int -> int
	val of_utf16: ?illegal_sequence:exn -> utf16_string -> t
	val of_utf32: ?illegal_sequence:exn -> utf32_string -> t
	val of_array: elm array -> t
end
module UTF16: sig
	type elm = utf16_char
	type t = utf16_string
	external compare: t -> t -> int = "%compare"
	external length: t -> int = "%caml_ba_dim_1"
	external get: t -> int -> elm = "%caml_ba_ref_1"
	external unsafe_get: t -> int -> elm = "%caml_ba_unsafe_ref_1"
	external set: t -> int -> elm -> unit = "%caml_ba_set_1"
	external unsafe_set: t -> int -> elm -> unit = "%caml_ba_unsafe_set_1"
	val empty: t
	val create: int -> t
	val copy: t -> t
	val append: t -> t -> t
	external sub: t -> int -> int -> t = "caml_ba_sub"
	external fill: t -> elm -> unit = "caml_ba_fill"
	external blit: t -> t -> unit = "caml_ba_blit"
	val sequence: ?illegal_sequence:exn -> elm -> int
	val max_sequence: int
	val get_code: ?illegal_sequence:exn -> t -> int ref -> int
	val set_code: ?illegal_sequence:exn -> t -> int ref -> int -> unit
	val lead: t -> int -> int
	val of_utf8: ?illegal_sequence:exn -> utf8_string -> t
	val of_utf32: ?illegal_sequence:exn -> utf32_string -> t
	val of_array: elm array -> t
end
module UTF32: sig
	type elm = utf32_char
	type t = utf32_string
	external compare: t -> t -> int = "%compare"
	external length: t -> int = "%caml_ba_dim_1"
	external get: t -> int -> elm = "%caml_ba_ref_1"
	external unsafe_get: t -> int -> elm = "%caml_ba_unsafe_ref_1"
	external set: t -> int -> elm -> unit = "%caml_ba_set_1"
	external unsafe_set: t -> int -> elm -> unit = "%caml_ba_unsafe_set_1"
	val empty: t
	val create: int -> t
	val copy: t -> t
	val append: t -> t -> t
	external sub: t -> int -> int -> t = "caml_ba_sub"
	external fill: t -> elm -> unit = "caml_ba_fill"
	external blit: t -> t -> unit = "caml_ba_blit"
	val sequence: ?illegal_sequence:exn -> elm -> int
	val max_sequence: int
	val get_code: ?illegal_sequence:exn -> t -> int ref -> int
	val set_code: ?illegal_sequence:exn -> t -> int ref -> int -> unit
	val lead: t -> int -> int
	val of_utf8: ?illegal_sequence:exn -> utf8_string -> t
	val of_utf16: ?illegal_sequence:exn -> utf16_string -> t
	val of_array: elm array -> t
end
