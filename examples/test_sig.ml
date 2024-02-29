module type ES = sig
	type elt
	val sequence: ?illegal_sequence:exn -> elt -> int
	val max_sequence: int
	val is_trailing: elt -> bool
	val decode3: ?illegal_sequence:exn -> ('d -> 'e -> elt) -> ('d -> 'e -> 'e) ->
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
	val empty: t
	val append: t -> t -> t
	val sub: t -> int -> int -> t
	val blit: t -> int -> mutable_t -> int -> int -> unit
	val lead: t -> int -> int
	val rear: t -> int -> int
	val get_code: ?illegal_sequence:exn -> t -> int ref -> Uchar.t
	val set_code: ?illegal_sequence:exn -> mutable_t -> int ref -> Uchar.t ->
		unit
	val of_array: elt array -> t
end;;

module type MS = sig
	type t
	include TS with type t := t and type mutable_t := t
	val set: t -> int -> elt -> unit
	val unsafe_set: t -> int -> elt -> unit
	val create: int -> t
	val copy: t -> t
	val fill: t -> int -> int -> elt -> unit
end;; (* mutable version of TS *)

open Unicode;;

let (_: unit) =
	let module Check: ES = UTF8 in
	let module Check: TS with type mutable_t := bytes = UTF8 in ();;
let (_: unit) =
	let module Check: MS = UTF8_Bytes in ();;
let (_: unit) =
	let module Check: ES = UTF16 in
	let module Check: MS = UTF16 in ();;
let (_: unit) =
	let module Check: ES = UTF32 in
	let module Check: MS = UTF32 in ();;

(* report *)

prerr_endline "ok";;
