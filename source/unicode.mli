module Uint32: sig
	type t [@@ocaml.immediate64]
	val of_int: int -> t
	val of_int32: int32 -> t
	val to_int: t -> int
	val to_int32: t -> int32
end
type utf8_char = char
type utf8_string = string
type utf16_char = int
type utf16_string =
	(int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type utf32_char = Uint32.t
type utf32_string =
	(int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t
val utf8_sequence: ?illegal_sequence:exn -> utf8_char -> int
val utf8_decode: ?illegal_sequence:exn -> ('d -> 'e -> utf8_char) ->
	('d -> 'e -> 'e) -> ('d -> 'e -> bool) -> 'a -> 'b -> 'c -> 'd -> 'e ->
	('a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) -> 'f
val utf8_get_code: ?illegal_sequence:exn -> utf8_string -> int ref -> Uchar.t
val utf8_lead: utf8_string -> int -> int
val utf8_encode: ?illegal_sequence:exn -> ('a -> 'b -> utf8_char -> 'b) ->
	'a -> 'b -> Uchar.t -> 'b
val utf8_set_code: ?illegal_sequence:exn -> bytes -> int ref -> Uchar.t -> unit
	[@@ocaml.deprecated]
val utf16_sequence: ?illegal_sequence:exn -> utf16_char -> int
val utf16_decode: ?illegal_sequence:exn -> ('d -> 'e -> utf16_char) ->
	('d -> 'e -> 'e) -> ('d -> 'e -> bool) -> 'a -> 'b -> 'c -> 'd -> 'e ->
	('a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) -> 'f
val utf16_get_code: ?illegal_sequence:exn -> utf16_string -> int ref -> Uchar.t
val utf16_lead: utf16_string -> int -> int
val utf16_encode: ?illegal_sequence:exn -> ('a -> 'b -> utf16_char -> 'b) ->
	'a -> 'b -> Uchar.t -> 'b
val utf16_set_code: ?illegal_sequence:exn -> utf16_string -> int ref ->
	Uchar.t -> unit
val utf32_sequence: ?illegal_sequence:exn -> utf32_char -> int
val utf32_decode: ?illegal_sequence:exn -> ('d -> 'e -> utf32_char) ->
	('d -> 'e -> 'e) -> ('d -> 'e -> bool) -> 'a -> 'b -> 'c -> 'd -> 'e ->
	('a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) -> 'f
val utf32_get_code: ?illegal_sequence:exn -> utf32_string -> int ref -> Uchar.t
val utf32_lead: utf32_string -> int -> int
val utf32_encode: ?illegal_sequence:exn -> ('a -> 'b -> utf32_char -> 'b) ->
	'a -> 'b -> Uchar.t -> 'b
val utf32_set_code: ?illegal_sequence:exn -> utf32_string -> int ref ->
	Uchar.t -> unit
val utf8_of_utf16: ?illegal_sequence:exn -> utf16_string -> utf8_string
val utf8_of_utf32: ?illegal_sequence:exn -> utf32_string -> utf8_string
val utf16_of_utf8: ?illegal_sequence:exn -> utf8_string -> utf16_string
val utf16_of_utf32: ?illegal_sequence:exn -> utf32_string -> utf16_string
val utf32_of_utf8: ?illegal_sequence:exn -> utf8_string -> utf32_string
val utf32_of_utf16: ?illegal_sequence:exn -> utf16_string -> utf32_string
module UTF8: sig
	type elt = utf8_char
	type t = utf8_string
	val compare: t -> t -> int
	external length: t -> int = "%string_length"
	external get: t -> int -> elt = "%string_safe_get"
	external unsafe_get: t -> int -> elt = "%string_unsafe_get"
	external set: bytes -> int -> elt -> unit = "%string_safe_set"
		[@@ocaml.deprecated]
	external unsafe_set: bytes -> int -> elt -> unit = "%string_unsafe_set"
		[@@ocaml.deprecated]
	val empty: t
	external create: int -> bytes = "caml_create_string" [@@ocaml.deprecated]
	val copy: t -> t [@@ocaml.deprecated]
	val append: t -> t -> t
	val sub: t -> int -> int -> t
	val fill: bytes -> int -> int -> elt -> unit [@@ocaml.deprecated]
	external unsafe_fill: bytes -> int -> int -> elt -> unit = "caml_fill_string"
		[@@noalloc] [@@ocaml.deprecated]
	val blit: t -> int -> bytes -> int -> int -> unit [@@ocaml.deprecated]
	external unsafe_blit: t -> int -> bytes -> int -> int -> unit =
		"caml_blit_string" [@@noalloc] [@@ocaml.deprecated]
	val sequence: ?illegal_sequence:exn -> elt -> int
	val max_sequence: int
	val decode: ?illegal_sequence:exn -> ('d -> 'e -> elt) -> ('d -> 'e -> 'e) ->
		('d -> 'e -> bool) -> 'a -> 'b -> 'c -> 'd -> 'e ->
		('a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) -> 'f
	val get_code: ?illegal_sequence:exn -> t -> int ref -> Uchar.t
	val lead: t -> int -> int
	val encode: ?illegal_sequence:exn -> ('a -> 'b -> elt -> 'b) -> 'a -> 'b ->
		Uchar.t -> 'b
	val set_code: ?illegal_sequence:exn -> bytes -> int ref -> Uchar.t -> unit
		[@@ocaml.deprecated]
	val of_utf16: ?illegal_sequence:exn -> utf16_string -> t
	val of_utf32: ?illegal_sequence:exn -> utf32_string -> t
	val of_array: elt array -> t
end
module UTF16: sig
	type elt = utf16_char
	type t = utf16_string
	external compare: t -> t -> int = "%compare"
	external length: t -> int = "%caml_ba_dim_1"
	external get: t -> int -> elt = "%caml_ba_ref_1"
	external unsafe_get: t -> int -> elt = "%caml_ba_unsafe_ref_1"
	external set: t -> int -> elt -> unit = "%caml_ba_set_1"
	external unsafe_set: t -> int -> elt -> unit = "%caml_ba_unsafe_set_1"
	val empty: t
	val create: int -> t
	val copy: t -> t
	val append: t -> t -> t
	external sub: t -> int -> int -> t = "caml_ba_sub"
	val fill: t -> int -> int -> elt -> unit
	val blit: t -> int -> t -> int -> int -> unit
	val sequence: ?illegal_sequence:exn -> elt -> int
	val max_sequence: int
	val decode: ?illegal_sequence:exn -> ('d -> 'e -> elt) -> ('d -> 'e -> 'e) ->
		('d -> 'e -> bool) -> 'a -> 'b -> 'c -> 'd -> 'e ->
		('a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) -> 'f
	val get_code: ?illegal_sequence:exn -> t -> int ref -> Uchar.t
	val lead: t -> int -> int
	val encode: ?illegal_sequence:exn -> ('a -> 'b -> elt -> 'b) -> 'a -> 'b ->
		Uchar.t -> 'b
	val set_code: ?illegal_sequence:exn -> t -> int ref -> Uchar.t -> unit
	val of_utf8: ?illegal_sequence:exn -> utf8_string -> t
	val of_utf32: ?illegal_sequence:exn -> utf32_string -> t
	val of_array: elt array -> t
end
module UTF32: sig
	type elt = int32
	type t = utf32_string
	external compare: t -> t -> int = "%compare"
	external length: t -> int = "%caml_ba_dim_1"
	external get: t -> int -> elt = "%caml_ba_ref_1"
	external unsafe_get: t -> int -> elt = "%caml_ba_unsafe_ref_1"
	external set: t -> int -> elt -> unit = "%caml_ba_set_1"
	external unsafe_set: t -> int -> elt -> unit = "%caml_ba_unsafe_set_1"
	val empty: t
	val create: int -> t
	val copy: t -> t
	val append: t -> t -> t
	external sub: t -> int -> int -> t = "caml_ba_sub"
	val fill: t -> int -> int -> elt -> unit
	val blit: t -> int -> t -> int -> int -> unit
	val sequence: ?illegal_sequence:exn -> utf32_char -> int
	val max_sequence: int
	val decode: ?illegal_sequence:exn -> ('d -> 'e -> utf32_char) ->
		('d -> 'e -> 'e) -> ('d -> 'e -> bool) -> 'a -> 'b -> 'c -> 'd -> 'e ->
		('a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) -> 'f
	val get_code: ?illegal_sequence:exn -> t -> int ref -> Uchar.t
	val lead: t -> int -> int
	val encode: ?illegal_sequence:exn -> ('a -> 'b -> utf32_char -> 'b) -> 'a ->
		'b -> Uchar.t -> 'b
	val set_code: ?illegal_sequence:exn -> t -> int ref -> Uchar.t -> unit
	val of_utf8: ?illegal_sequence:exn -> utf8_string -> t
	val of_utf16: ?illegal_sequence:exn -> utf16_string -> t
	val of_array: elt array -> t
end
