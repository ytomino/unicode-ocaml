module type ES = sig
	type elt
	val sequence: ?illegal_sequence:exn -> elt -> int
	val max_sequence: int
	val is_trailing: elt -> bool
	val decode: ?illegal_sequence:exn -> ('d -> 'e -> elt) -> ('d -> 'e -> 'e) ->
		('d -> 'e -> bool) -> 'a -> 'b -> 'c -> 'd -> 'e ->
		('a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) -> 'f
	val encode: ?illegal_sequence:exn -> ('a -> 'b -> elt -> 'b) -> 'a -> 'b ->
		Uchar.t -> 'b
end;;

module type TS = sig
	type elt
	type t
	type mutable_t
	val compare: t -> t -> int
	val length: t -> int
	val get: t -> int -> elt
	val unsafe_get: t -> int -> elt
	val set: mutable_t -> int -> elt -> unit
	val unsafe_set: mutable_t -> int -> elt -> unit
	val empty: t
	val create: int -> mutable_t
	val copy: t -> t
	val append: t -> t -> t
	val sub: t -> int -> int -> t
	val fill: mutable_t -> int -> int -> elt -> unit
	val blit: t -> int -> mutable_t -> int -> int -> unit
	val get_code: ?illegal_sequence:exn -> t -> int ref -> Uchar.t
	val lead: t -> int -> int
	val set_code: ?illegal_sequence:exn -> mutable_t -> int ref -> Uchar.t ->
		unit
	val of_array: elt array -> t
end;;

open Unicode;;

let (_: unit) =
	let module Check: ES = UTF8 in
	let module Check: TS with type mutable_t := bytes = UTF8 in ();;
let (_: unit) =
	let module Check: TS with type mutable_t := bytes = UTF8_Bytes in ();;
let (_: unit) =
	let module Check: ES = UTF16 in
	let module Check: TS with type mutable_t := utf16_string = UTF16 in ();;
let (_: unit) =
	let module Check: ES = UTF32 in
	let module Check: TS with type mutable_t := utf32_string = UTF32 in ();;

(* report *)

prerr_endline "ok";;
