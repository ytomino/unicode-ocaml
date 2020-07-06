module type S = sig
	type elm
	type t
	type mutable_t
	val compare: t -> t -> int
	val length: t -> int
	val get: t -> int -> elm
	val unsafe_get: t -> int -> elm
	val set: mutable_t -> int -> elm -> unit
	val unsafe_set: mutable_t -> int -> elm -> unit
	val empty: t
	val create: int -> mutable_t
	val copy: t -> t
	val append: t -> t -> t
	val sub: t -> int -> int -> t
	val fill: mutable_t -> int -> int -> elm -> unit
	val blit: t -> int -> mutable_t -> int -> int -> unit
	val sequence: ?illegal_sequence:exn -> elm -> int
	val max_sequence: int
	val decode: ?illegal_sequence:exn -> ('d -> 'e -> elm) -> ('d -> 'e -> 'e) ->
		('d -> 'e -> bool) -> 'a -> 'b -> 'c -> 'd -> 'e ->
		('a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) -> 'f
	val get_code: ?illegal_sequence:exn -> t -> int ref -> Uchar.t
	val lead: t -> int -> int
	val encode: ?illegal_sequence:exn -> ('a -> 'b -> elm -> 'b) -> 'a -> 'b ->
		Uchar.t -> 'b
	val set_code: ?illegal_sequence:exn -> mutable_t -> int ref -> Uchar.t ->
		unit
	val of_array: elm array -> t
end;;

open Unicode;;

let (_: unit) =
	let module Check: (S with type mutable_t := bytes) = UTF8 in ();;
let (_: unit) =
	let module Check: (S with type mutable_t := utf16_string) = UTF16 in ();;
let (_: unit) =
	let module Check: (S with type mutable_t := utf32_string) = UTF32 in ();;

(* report *)

prerr_endline "ok";;
