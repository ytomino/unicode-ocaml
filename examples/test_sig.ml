module type ES = sig
	type elt
	val sequence:
		fail:([> `illegal_sequence | `surrogate_fragment of int] -> int) -> elt -> int
	val max_sequence: int
	val is_trailing: elt -> bool
	val decode3: ('d -> 'e -> elt) -> ('d -> 'e -> 'e) -> ('d -> 'e -> bool) ->
		('a -> 'b -> 'c -> 'd -> 'e -> 'e -> Uchar.t -> 'f) ->
		fail:('a -> 'b -> 'c -> 'd -> 'e -> 'e ->
			[>
				| `illegal_sequence
				| `over_17planes of int
				| `overly_long of
					[`over_17planes of int | `some of Uchar.t | `surrogate_fragment of int]
				| `surrogate_fragment of int
				| `truncated
			] -> 'f
		) ->
		'a -> 'b -> 'c -> 'd -> 'e -> 'f
	val decode: ('a -> 'b -> elt) -> ('a -> 'b -> 'b) -> ('a -> 'b -> bool) ->
		('a -> 'b -> 'b -> Uchar.t -> 'c) ->
		fail:('a -> 'b -> 'b ->
			[>
				| `illegal_sequence
				| `over_17planes of int
				| `overly_long of
					[`over_17planes of int | `some of Uchar.t | `surrogate_fragment of int]
				| `surrogate_fragment of int
				| `truncated
			] -> 'c
		) ->
		'a -> 'b -> 'c
	val encode4: ('e -> 'f -> elt -> 'f) ->
		fail:('a -> 'b -> 'c -> 'd -> 'e -> 'f -> [> `unexist] -> 'f) ->
		'a -> 'b -> 'c -> 'd -> 'e -> 'f -> Uchar.t -> 'f
	val encode: ('a -> 'b -> elt -> 'b) ->
		fail:('a -> 'b -> [> `unexist ] -> 'b) -> 'a -> 'b -> Uchar.t -> 'b
end;;

module type TS = sig
	type elt
	type t
	type mutable_t
	val compare: t -> t -> int
	val equal: t -> t -> bool
	val length: t -> int
	val get: t -> int -> elt
	val unsafe_get: t -> int -> elt
	val empty: t
	val cat: t -> t -> t
	val sub: t -> int -> int -> t
	val blit: t -> int -> mutable_t -> int -> int -> unit
	val lead: t -> int -> int
	val rear: t -> int -> int
	val get_code:
		fail:(t -> int -> int ->
			[>
				| `illegal_sequence
				| `over_17planes of int
				| `overly_long of
					[`over_17planes of int | `some of Uchar.t | `surrogate_fragment of int]
				| `surrogate_fragment of int
				| `truncated
			] -> Uchar.t
		) ->
		t -> int ref -> Uchar.t
	val set_code: fail:(mutable_t -> int -> [> `unexist] -> Uchar.t) ->
		mutable_t -> int ref -> Uchar.t -> unit
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

let _: unit =
	let module Check: ES = UTF8 in
	let module Check: TS with type mutable_t := bytes = UTF8 in ();;
let _: unit =
	let module Check: MS = UTF8_Bytes in ();;
let _: unit =
	let module Check: ES = UTF16 in
	let module Check: MS = UTF16 in ();;
let _: unit =
	let module Check: ES = UTF32 in
	let module Check: MS = UTF32 in ();;

(* report *)

prerr_endline "ok";;
