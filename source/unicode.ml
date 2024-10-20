let slice a start len = (
	if start = 0 && len = Bigarray.Array1.dim a then a else
	Bigarray.Array1.sub a start len
);;

let ba_copy source = (
	let result = Bigarray.Array1.create (Bigarray.Array1.kind source)
		Bigarray.c_layout (Bigarray.Array1.dim source)
	in
	Bigarray.Array1.blit source result;
	result
);;

let ba_cat x y = (
	let x_length = Bigarray.Array1.dim x in
	let y_length = Bigarray.Array1.dim y in
	let result = Bigarray.Array1.create (Bigarray.Array1.kind x) Bigarray.c_layout
		(x_length + y_length)
	in
	Bigarray.Array1.blit x (slice result 0 x_length);
	Bigarray.Array1.blit y (slice result x_length y_length);
	result
);;

let ba_fill dest start len c = (
	Bigarray.Array1.fill (slice dest start len) c
);;

let ba_blit source srcoff dest destoff len = (
	Bigarray.Array1.blit (slice source srcoff len) (slice dest destoff len)
);;

let bytes_of_array source = (
	let length = Array.length source in
	let result = Bytes.create length in
	for i = 0 to length - 1 do
		Bytes.unsafe_set result i (Array.unsafe_get source i)
	done;
	result
);;

let string_end source index = (
	index >= String.length source
);;

let ba_end source index = (
	index >= Bigarray.Array1.dim source
);;

let id3_snd _ x _ = x;;
let id3 _ _ x = x;;
let id4 _ _ _ x = x;;

let one _ = 1;;

let succ_snd _: int -> int = succ;;

let finish_get_code _ _ (c: int ref) _ _ (e': int) (result: Uchar.t) = (
	c := e';
	result
);;

let fail_decode (type t error) _ (fail: t -> int -> int -> error -> Uchar.t)
	(index: int ref) (source: t) (pos: int) (end_pos: int) (error: error) =
(
	let alt = fail source pos end_pos error in
	finish_get_code () () index () () end_pos alt
);;

let fail_encode (loc: string) _ _ _ _ _ _ _ = failwith loc;;

let is_in_17planes (code: int) = (
	code <= 0x10ffff && (Sys.word_size > 32 || code >= 0)
);;

let is_not_surrogate_fragment (code: int) = (
	code land lnot 0x07ff <> 0xd800
);;

module type Uint32_S = sig
	type t [@@ocaml.immediate64]
	val zero: t
	val compare: t -> t -> int
	val is_uint31: t -> bool
	val of_int: int -> t
	val of_int32: int32 -> t
	val to_int: t -> int
	val to_int32: t -> int32
	val unsafe_get: t array -> int -> t (* specialized *)
end;;

module Non_immediate_Uint32 = struct
	type t = int32;;
	let zero = Int32.zero
	let compare = Int32.unsigned_compare;;
	let is_uint31 x = x >= 0l;;
	let of_int x = (
		if Sys.word_size <= 32 then Int32.logand (Int32.of_int x) 0x7fffffffl else
		Int32.of_int x
	);;
	let of_int32 x = x;;
	let to_int x = (
		if Sys.word_size <= 32 then Int32.to_int x else
		Int32.to_int x land (1 lsl 32 - 1)
	);;
	let to_int32 x = x;;
	let unsafe_get: t array -> int -> t = Array.unsafe_get;;
end;;

module Immediate_Uint32 = struct
	type t = int;;
	let zero = Int.zero
	let compare = Int.compare;;
	let is_uint31 x = x land lnot 0x7fffffff = 0;;
	let of_int x = x land (1 lsl 32 - 1);;
	let of_int32 x = Int32.to_int x land (1 lsl 32 - 1);;
	let to_int x = x;;
	let to_int32 = Int32.of_int;;
	let unsafe_get: t array -> int -> t = Array.unsafe_get;;
end;;

module Uint32: Uint32_S = struct
	include Sys.Immediate64.Make (Immediate_Uint32) (Non_immediate_Uint32);;
	module type S = Uint32_S with type t := t;;
	include (val
		match repr with
		| Immediate -> (module Immediate_Uint32: S)
		| Non_immediate -> (module Non_immediate_Uint32: S)
	);;
end;;

type utf8_char = char;;
type utf8_string = string;;

type utf16_char = int;;
type utf16_string =
	(int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;;

type utf32_char = Uint32.t;;
type utf32_string =
	(int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;;

let min (x: int) (y: int) = if x <= y then x else y;; (* specialized *)

let utf8_get: utf8_string -> int -> utf8_char = String.get;;

let utf8_add (dest: bytes) (index: int) (item: utf8_char) = (
	Bytes.set dest index item;
	index + 1
);;

let utf8_storing_length (code: int) = (
	if code land lnot 0x7f = 0 then 1 else
	if code land lnot (1 lsl (5 + 6) - 1) = 0 then 2 else
	if code land lnot (1 lsl (4 + 12) - 1) = 0 then 3 else
	if code land lnot (1 lsl (3 + 18) - 1) = 0 then 4 else
	if code land lnot (1 lsl (2 + 24) - 1) = 0 then 5 else
	6
);;

type utf8_decode_error = [
	| `illegal_sequence
	| `over_17planes of int
	| `overly_long of
		[`over_17planes of int | `some of Uchar.t | `surrogate_fragment of int]
	| `surrogate_fragment of int
	| `truncated
];;

let utf8_sequence ~(fail: [> `illegal_sequence] -> int) (lead: utf8_char) = (
	let lead = int_of_char lead in
	if lead land 0b10000000 = 0 then 1 else
	if lead land 0b11100000 = 0b11000000 then 2 else
	if lead land 0b11110000 = 0b11100000 then 3 else
	if lead land 0b11111000 = 0b11110000 then 4 else
	if lead land 0b11111100 = 0b11111000 then 5 else
	if lead land 0b11111110 = 0b11111100 then 6
	else fail `illegal_sequence
);;

let utf8_is_trailing (item: utf8_char) = (
	int_of_char item land 0b11000000 = 0b10000000
);;

let utf8_decode3: type a b c d e f. (d -> e -> utf8_char) -> (d -> e -> e) ->
	(d -> e -> bool) -> (a -> b -> c -> d -> e -> e -> Uchar.t -> f) ->
	fail:(a -> b -> c -> d -> e -> e -> [> utf8_decode_error] -> f) ->
	a -> b -> c -> d -> e -> f =
	let rec tails get_f inc_f end_f fail cont_f a b c d e e' length offset code = (
		if not (end_f d e') then (
			let tail = get_f d e' in
			if utf8_is_trailing tail then (
				let tail = int_of_char tail in
				let e' = inc_f d e' in
				let code = code lsl 6 lor (tail land 0b00111111) in
				let offset = offset - 6 in
				if offset <= 0 then (
					(* Finish. *)
					if utf8_storing_length code = length then (
						if is_in_17planes code then (
							if is_not_surrogate_fragment code
							then cont_f a b c d e e' (Uchar.unsafe_of_int code)
							else fail a b c d e e' (`surrogate_fragment code)
						) else fail a b c d e e' (`over_17planes code)
					) else
						fail a b c d e e' (
							`overly_long (
								if is_in_17planes code then (
									if is_not_surrogate_fragment code then `some (Uchar.unsafe_of_int code)
									else `surrogate_fragment code
								) else `over_17planes code
							)
						)
				) else tails get_f inc_f end_f fail cont_f a b c d e e' length offset code
			) else fail a b c d e e' `truncated
		) else fail a b c d e e' `truncated
	) in
	fun get_f inc_f end_f cont_f ~fail a b c d e ->
	let lead = int_of_char (get_f d e) in
	let e' = inc_f d e in
	if lead land 0b10000000 = 0 then (
		cont_f a b c d e e' (Uchar.unsafe_of_int lead)
	) else if lead land 0b11100000 = 0b11000000 then (
		tails get_f inc_f end_f fail cont_f a b c d e e' 2 6 (lead land 0b00011111)
	) else if lead land 0b11110000 = 0b11100000 then (
		tails get_f inc_f end_f fail cont_f a b c d e e' 3 12 (lead land 0b00001111)
	) else if lead land 0b11111000 = 0b11110000 then (
		tails get_f inc_f end_f fail cont_f a b c d e e' 4 18 (lead land 0b00000111)
	) else if lead land 0b11111100 = 0b11111000 then (
		tails get_f inc_f end_f fail cont_f a b c d e e' 5 24 (lead land 0b00000011)
	) else if lead land 0b11111110 = 0b11111100 then (
		tails get_f inc_f end_f fail cont_f a b c d e e' 6 30 (lead land 0b00000001)
	) else fail a b c d e e' `illegal_sequence;;

let utf8_decode (type a b c) (get_f: a -> b -> utf8_char) (inc_f: a -> b -> b)
	(end_f: a -> b -> bool) (cont_f: a -> b -> b -> Uchar.t -> c)
	~(fail: a -> b -> b -> [> utf8_decode_error] -> c) (a: a) (b: b) =
(
	utf8_decode3 get_f inc_f end_f id3_snd ~fail:id3 () cont_f fail a b
);;

let utf8_encode4: type a b c d e f. (e -> f -> utf8_char -> f) ->
	fail:(a -> b -> c -> d -> e -> f -> [> `unexist] -> f) ->
	a -> b -> c -> d -> e -> f -> Uchar.t -> f =
	let rec tails add_f offset e f code = (
		let offset = offset - 6 in
		let f =
			add_f e f (char_of_int (code lsr offset land 0b00111111 lor 0b10000000))
		in
		if offset <= 0 then f
		else tails add_f offset e f code
	) in
	fun add_f ~fail a b c d e f code ->
	(* without checking surrogate pair in set *)
	let code = Uchar.to_int code in
	let mask32 =
		if Sys.word_size <= 32 then lnot 0
		else 1 lsl 32 - 1 (* mask Uchar.t to 32bit range, consistency with UTF-32 *)
	in
	if code land (lnot 0x7fffffff land mask32) = 0 then (
		let code = code land mask32 in
		let length = utf8_storing_length code in
		if length = 1 then add_f e f (char_of_int code) else
		let offset = (length - 1) * 6 in
		let mask = (1 lsl (8 - length) - 1) in
		let leading = char_of_int
			(code lsr offset land (mask lsr 1) lor (0xff lxor mask))
		in
		let f = add_f e f leading in
		tails add_f offset e f code
	) else fail a b c d e f `unexist;; (* only when Sys.word_size > 32 *)

let utf8_encode (type a b) (add_f: a -> b -> utf8_char -> b)
	~(fail: a -> b -> [> `unexist] -> b) (a: a) (b: b) (code: Uchar.t) =
(
	utf8_encode4 add_f ~fail:id4 () () () fail a b code
);;

let utf8_lead: utf8_string -> int -> int =
	let rec lead source index j c = (
		if not (utf8_is_trailing c) || j <= 0 || j + 5 <= index then (
			if j + utf8_sequence ~fail:one c > index then j else
			index (* illegal *)
		) else
		let n = j - 1 in
		lead source index n (String.unsafe_get source n)
	) in
	fun source index -> lead source index index (String.get source index);;

let utf8_rear: utf8_string -> int -> int =
	let rec rear source j e = (
		let n = j + 1 in
		if n >= e || not (utf8_is_trailing (String.unsafe_get source n)) then j else
		rear source n e
	) in
	let rec lead source index j c = (
		if not (utf8_is_trailing c) || j <= 0 || j + 5 <= index then (
			let e = j + utf8_sequence ~fail:one c in
			if e > index then rear source index (min e (String.length source)) else
			index (* illegal *)
		) else
		let n = j - 1 in
		lead source index n (String.unsafe_get source n)
	) in
	fun source index -> lead source index index (String.get source index);;

let utf8_get_code
	~(fail: utf8_string -> int -> int -> [> utf8_decode_error] -> Uchar.t)
	(source: utf8_string) (index: int ref) =
(
	utf8_decode3 utf8_get succ_snd string_end finish_get_code ~fail:fail_decode
		() fail index source !index
);;

let utf8_set_code ~(fail: bytes -> int -> [> `unexist] -> Uchar.t)
	(dest: bytes) (index: int ref) (code: Uchar.t) =
(
	index :=
		utf8_encode4 utf8_add
			~fail:(fun _ _ _ fail dest index error ->
				let alt = fail dest index error in
				utf8_encode4 utf8_add ~fail:(fail_encode "utf8_set_code")
					() () () () dest index alt
			)
			() () () fail dest !index code
);;

let utf16_get: utf16_string -> int -> utf16_char = Bigarray.Array1.get;;

let utf16_add (dest: utf16_string) (index: int) (item: utf16_char) = (
	Bigarray.Array1.set dest index item;
	index + 1
);;

let utf16_is_leading_1 (item: utf16_char) = (
	item land 0xf800 <> 0xd800 (* mask utf16_char to 16bit range *)
);;

let utf16_is_leading_2 (item: utf16_char) = (
	item land 0xfc00 = 0xd800 (* mask utf16_char to 16bit range *)
);;

type utf16_decode_error = [`surrogate_fragment of int];;

let utf16_sequence ~(fail: [> `surrogate_fragment of int] -> int)
	(lead: utf16_char) =
(
	if utf16_is_leading_1 lead then 1 else
	if utf16_is_leading_2 lead then 2
	else fail (`surrogate_fragment lead)
);;

let utf16_is_trailing (item: utf16_char) = (
	item land 0xfc00 = 0xdc00 (* mask utf16_char to 16bit range *)
);;

let utf16_decode3 (type a b c d e f) (get_f: d -> e -> utf16_char)
	(inc_f: d -> e -> e) (end_f: d -> e -> bool)
	(cont_f: a -> b -> c -> d -> e -> e -> Uchar.t -> f)
	~(fail: a -> b -> c -> d -> e -> e -> [> utf16_decode_error] -> f)
	(a: a) (b: b) (c: c) (d: d) (e: e) =
(
	let lead = get_f d e in
	let lead = lead land 0xffff in (* mask utf16_char to 16bit range *)
	let e' = inc_f d e in
	if utf16_is_leading_1 lead then (
		cont_f a b c d e e' (Uchar.unsafe_of_int lead)
	) else if utf16_is_leading_2 lead then (
		if not (end_f d e') then (
			let tail = get_f d e' in
			let tail = tail land 0xffff in (* mask utf16_char to 16bit range *)
			if utf16_is_trailing tail then (
				let e' = inc_f d e' in
				let result =
					((lead land (1 lsl 10 - 1)) lsl 10 lor (tail land (1 lsl 10 - 1))) + 0x10000
				in
				cont_f a b c d e e' (Uchar.unsafe_of_int result)
			) else fail a b c d e e' (`surrogate_fragment lead)
		) else fail a b c d e e' (`surrogate_fragment lead)
	) else fail a b c d e e' (`surrogate_fragment lead)
);;

let utf16_decode (type a b c) (get_f: a -> b -> utf16_char)
	(inc_f: a -> b -> b) (end_f: a -> b -> bool)
	(cont_f: a -> b -> b -> Uchar.t -> c)
	~(fail: a -> b -> b -> [> utf16_decode_error] -> c) (a: a) (b: b) =
(
	utf16_decode3 get_f inc_f end_f id3_snd ~fail:id3 () cont_f fail a b
);;

let utf16_encode4 (type a b c d e f) (add_f: e -> f -> utf16_char -> f)
	~(fail: a -> b -> c -> d -> e -> f -> [> `unexist] -> f)
	(a: a) (b: b) (c: c) (d: d) (e: e) (f: f) (code: Uchar.t) =
(
	(* without checking surrogate pair in set *)
	let code = Uchar.to_int code in
	let mask32 =
		if Sys.word_size <= 32 then lnot 0
		else 1 lsl 32 - 1 (* mask Uchar.t to 32bit range, consistency with UTF-32 *)
	in
	if code land (lnot 0xffff land mask32) = 0 then add_f e f code else
	let code' = code - 0x10000 in
	if code' land (lnot 0xfffff land mask32) = 0 then (
		let f = add_f e f (0xd800 lor ((code' lsr 10) land (1 lsl 10 - 1))) in
		add_f e f (0xdc00 lor (code' land (1 lsl 10 - 1)))
	) else fail a b c d e f `unexist
);;

let utf16_encode (type a b) (add_f: a -> b -> utf16_char -> b)
	~(fail: a -> b -> [> `unexist] -> b) (a: a) (b: b) (code: Uchar.t) =
(
	utf16_encode4 add_f ~fail:id4 () () () fail a b code
);;

let utf16_lead (source: utf16_string) (index: int) = (
	let c = Bigarray.Array1.get source index in
	if not (utf16_is_trailing c) || index <= 0 then index else
	let n = index - 1 in
	let p = Bigarray.Array1.unsafe_get source n in
	if utf16_is_leading_2 p then n else
	index (* illegal *)
);;

let utf16_rear (source: utf16_string) (index: int) = (
	let c = Bigarray.Array1.get source index in
	if not (utf16_is_leading_2 c) then index else
	let n = index + 1 in
	if n >= Bigarray.Array1.dim source then index else
	let t = Bigarray.Array1.unsafe_get source n in
	if utf16_is_trailing t then n else
	index (* illegal *)
);;

let utf16_get_code
	~(fail: utf16_string -> int -> int -> [> utf16_decode_error] -> Uchar.t)
	(source: utf16_string) (index: int ref) =
(
	utf16_decode3 utf16_get succ_snd ba_end finish_get_code ~fail:fail_decode
		() fail index source !index
);;

let utf16_set_code ~(fail: utf16_string -> int -> [> `unexist] -> Uchar.t)
	(dest: utf16_string) (index: int ref) (code: Uchar.t) =
(
	index :=
		utf16_encode4 utf16_add
			~fail:(fun _ _ _ fail dest index error ->
				let alt = fail dest index error in
				utf16_encode4 utf16_add ~fail:(fail_encode "utf16_set_code")
					() () () () dest index alt
			)
			() () () fail dest !index code
);;

let utf32_get (source: utf32_string) (index: int) = (
	Uint32.of_int32 (Bigarray.Array1.get source index)
);;

let utf32_unsafe_get (source: utf32_string) (index: int) = (
	Uint32.of_int32 (Bigarray.Array1.unsafe_get source index)
);;

let utf32_add (dest: utf32_string) (index: int) (item: utf32_char) = (
	Bigarray.Array1.set dest index (Uint32.to_int32 item);
	index + 1
);;

type utf32_decode_error =
	[`illegal_sequence | `over_17planes of int | `surrogate_fragment of int];;

let utf32_sequence
	~(fail: [> `illegal_sequence | `surrogate_fragment of int] -> int)
	(lead: utf32_char) =
(
	if Uint32.is_uint31 lead then (
		let lead = Uint32.to_int lead in
		if is_not_surrogate_fragment lead then 1
		else fail (`surrogate_fragment lead)
	) else fail `illegal_sequence
);;

let utf32_is_trailing (_: utf32_char) = false;;

let utf32_decode3 (type a b c d e f) (get_f: d -> e -> utf32_char)
	(inc_f: d -> e -> e) (_: d -> e -> bool)
	(cont_f: a -> b -> c -> d -> e -> e -> Uchar.t -> f)
	~(fail: a -> b -> c -> d -> e -> e -> [> utf32_decode_error] -> f)
	(a: a) (b: b) (c: c) (d: d) (e: e) =
(
	let result = get_f d e in
	let e' = inc_f d e in
	if Uint32.is_uint31 result then (
		let result = Uint32.to_int result in
		if is_in_17planes result then (
			if is_not_surrogate_fragment result then (
				let result = Uchar.unsafe_of_int result in
				cont_f a b c d e e' result
			) else fail a b c d e e' (`surrogate_fragment result)
		) else fail a b c d e e' (`over_17planes result)
	) else fail a b c d e e' `illegal_sequence
);;

let utf32_decode (type a b c) (get_f: a -> b -> utf32_char)
	(inc_f: a -> b -> b) (end_f: a -> b -> bool)
	(cont_f: a -> b -> b -> Uchar.t -> c)
	~(fail: a -> b -> b -> [> utf32_decode_error] -> c) (a: a) (b: b) =
(
	utf32_decode3 get_f inc_f end_f id3_snd ~fail:id3 () cont_f fail a b
);;

let utf32_encode4 (type a b c d e f fail_f) (add_f: e -> f -> utf32_char -> f)
	~fail:(_: fail_f) (_: a) (_: b) (_: c) (_: d) (e: e) (f: f) (code: Uchar.t) =
(
	(* without checking surrogate pair in set *)
	let code = Uchar.to_int code in
	add_f e f (Uint32.of_int code)
);;

let utf32_encode (type a b) (add_f: a -> b -> utf32_char -> b)
	~(fail: a -> b -> [> ] -> b) (a: a) (b: b) (code: Uchar.t) =
(
	utf32_encode4 add_f ~fail:id3 () () () fail a b code
);;

let utf32_lead (_: utf32_string) (index: int) = index;;

let utf32_rear (_: utf32_string) (index: int) = index;;

let utf32_get_code
	~(fail: utf32_string -> int -> int -> [> utf32_decode_error] -> Uchar.t)
	(source: utf32_string) (index: int ref) =
(
	utf32_decode3 utf32_get succ_snd ba_end finish_get_code ~fail:fail_decode
		() fail index source !index
);;

let utf32_set_code ~fail:(_: utf32_string -> int -> [> ] -> Uchar.t)
	(dest: utf32_string) (index: int ref) (code: Uchar.t) =
(
	index := utf32_encode4 utf32_add ~fail:() () () () () dest !index code
);;

let utf8_of_utf16:
	fail:(utf16_string -> int -> int -> [> utf16_decode_error | `unexist] ->
		Uchar.t
	) ->
	utf16_string -> utf8_string =
	let rec encode_fail fail source old_i i result j error = (
		let code = fail source old_i i error in
		utf8_encode4 utf8_add ~fail:(fail_encode "utf8_of_utf16")
			() () () () result j code
	) and encode fail result j source old_i i code = (
		let j =
			utf8_encode4 utf8_add ~fail:encode_fail fail source old_i i result j code
		in
		decode fail result j source i
	) and decode_fail fail result j source old_i i error = (
		let code = fail source old_i i error in
		encode fail result j source old_i i code
	) and decode fail result j source i = (
		if i >= Bigarray.Array1.dim source
		then Bytes.unsafe_to_string (Bytes.sub result 0 j) else
		utf16_decode3 utf16_get succ_snd ba_end encode ~fail:decode_fail
			fail result j source i
	) in
	fun ~fail source ->
	let source_length = Bigarray.Array1.dim source in
	let result = Bytes.create (3 * source_length) in
	decode fail result 0 source 0;;

let utf8_of_utf32:
	fail:(utf32_string -> int -> int -> [> utf32_decode_error | `unexist] ->
		Uchar.t
	) ->
	utf32_string -> utf8_string =
	let rec encode_fail fail source old_i i result j error = (
		let code = fail source old_i i error in
		utf8_encode4 utf8_add ~fail:(fail_encode "utf8_of_utf32")
			() () () () result j code
	) and encode fail result j source old_i i code = (
		let j =
			utf8_encode4 utf8_add ~fail:encode_fail fail source old_i i result j code
		in
		decode fail result j source i
	) and decode_fail fail result j source old_i i error = (
		let code = fail source old_i i error in
		encode fail result j source old_i i code
	) and decode fail result j source i = (
		if i >= Bigarray.Array1.dim source
		then Bytes.unsafe_to_string (Bytes.sub result 0 j) else
		utf32_decode3 utf32_get succ_snd ba_end encode ~fail:decode_fail
			fail result j source i
	) in
	fun ~fail source ->
	let source_length = Bigarray.Array1.dim source in
	let result = Bytes.create (6 * source_length) in
	decode fail result 0 source 0;;

let utf16_of_utf8:
	fail:(utf8_string -> int -> int -> [> utf8_decode_error | `unexist] -> Uchar.t
	) ->
	utf8_string -> utf16_string =
	let rec encode_fail fail source old_i i result j error = (
		let code = fail source old_i i error in
		utf16_encode4 utf16_add ~fail:(fail_encode "utf16_of_utf8")
			() () () () result j code
	) and encode fail result j source old_i i code = (
		let j =
			utf16_encode4 utf16_add ~fail:encode_fail fail source old_i i result j code
		in
		decode fail result j source i
	) and decode_fail fail result j source old_i i error = (
		let code = fail source old_i i error in
		encode fail result j source old_i i code
	) and decode fail result j source i = (
		if i >= String.length source then Bigarray.Array1.sub result 0 j else
		utf8_decode3 utf8_get succ_snd string_end encode ~fail:decode_fail
			fail result j source i
	) in
	fun ~fail source ->
	let source_length = String.length source in
	let result =
		(* Conversion from UTF-8 to UTF-16 never increases the number of elements.
		   However, if an illegal sequence in UTF-8 is substituted to a character
		   outside of BMP, the number of elements may be increased. *)
		Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout
			(2 * source_length)
	in
	decode fail result 0 source 0;;

let utf16_of_utf32:
	fail:(utf32_string -> int -> int -> [> utf32_decode_error | `unexist] ->
		Uchar.t
	) ->
	utf32_string -> utf16_string =
	let rec encode_fail fail source old_i i result j error = (
		let code = fail source old_i i error in
		utf16_encode4 utf16_add ~fail:(fail_encode "utf16_of_utf32")
			() () () () result j code
	) and encode fail result j source old_i i code = (
		let j =
			utf16_encode4 utf16_add ~fail:encode_fail fail source old_i i result j code
		in
		decode fail result j source i
	) and decode_fail fail result j source old_i i error = (
		let code = fail source old_i i error in
		encode fail result j source old_i i code
	) and decode fail result j source i = (
		if i >= Bigarray.Array1.dim source then Bigarray.Array1.sub result 0 j else
		utf32_decode3 utf32_get succ_snd ba_end encode ~fail:decode_fail
			fail result j source i
	) in
	fun ~fail source ->
	let source_length = Bigarray.Array1.dim source in
	let result = Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout
		(2 * source_length)
	in
	decode fail result 0 source 0;;

let utf32_of_utf8:
	fail:(utf8_string -> int -> int -> [> utf8_decode_error] -> Uchar.t) ->
	utf8_string -> utf32_string =
	let rec encode fail result j source _ i code = (
		let j = utf32_encode4 utf32_add ~fail:() () () () () result j code in
		decode fail result j source i
	) and decode_fail fail result j source old_i i error = (
		let code = fail source old_i i error in
		encode fail result j source old_i i code
	) and decode fail result j source i = (
		if i >= String.length source then slice result 0 j else
		utf8_decode3 utf8_get succ_snd string_end encode ~fail:decode_fail
			fail result j source i
	) in
	fun ~fail source ->
	let source_length = String.length source in
	let result = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout
		source_length
	in
	decode fail result 0 source 0;;

let utf32_of_utf16:
	fail:(utf16_string -> int -> int -> [> utf16_decode_error] -> Uchar.t) ->
	utf16_string -> utf32_string =
	let rec encode fail result j source _ i code = (
		let j = utf32_encode4 utf32_add ~fail:() () () () () result j code in
		decode fail result j source i
	) and decode_fail fail result j source old_i i error = (
		let code = fail source old_i i error in
		encode fail result j source old_i i code
	) and decode fail result j source i = (
		if i >= Bigarray.Array1.dim source then slice result 0 j else
		utf16_decode3 utf16_get succ_snd ba_end encode ~fail:decode_fail
			fail result j source i
	) in
	fun ~fail source ->
	let source_length = Bigarray.Array1.dim source in
	let result = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout
		source_length
	in
	decode fail result 0 source 0;;

module UTF8 = struct
	type elt = utf8_char;;
	let sequence = utf8_sequence;;
	let max_sequence = 6;;
	let is_trailing = utf8_is_trailing;;
	let decode3 = utf8_decode3;;
	let decode = utf8_decode;;
	let encode4 = utf8_encode4;;
	let encode = utf8_encode;;
	include String;;
	let empty = "";;
	let cat = ( ^ );; (* String.cat is added since OCaml 4.13 *)
	let lead = utf8_lead;;
	let rear = utf8_rear;;
	let get_code = utf8_get_code;;
	let set_code = utf8_set_code;;
	let of_utf16 = utf8_of_utf16;;
	let of_utf32 = utf8_of_utf32;;
	let of_array (source: elt array) = (
		Bytes.unsafe_to_string (bytes_of_array source)
	);;
end;;

module UTF8_Bytes = struct
	type elt = utf8_char;;
	include Bytes;;
	let lead (source: t) (index: int) = (
		utf8_lead (unsafe_to_string source) index
	);;
	let rear (source: t) (index: int) = (
		utf8_rear (unsafe_to_string source) index
	);;
	let get_code ~(fail: t -> int -> int -> [> utf8_decode_error] -> Uchar.t)
		(source: t) (index: int ref) =
	(
		utf8_decode3 Bytes.get succ_snd
			(fun source index -> index >= Bytes.length source) finish_get_code
			~fail:fail_decode () fail index source !index
	);;
	let set_code = utf8_set_code;;
	let of_array = bytes_of_array;;
end;;

module type BA1_without_t = module type of Bigarray.Array1
	with type ('a, 'b, 'c) t := ('a, 'b, 'c) Bigarray.Array1.t;;

module UTF16 = struct
	type elt = utf16_char;;
	let sequence = utf16_sequence;;
	let max_sequence = 2;;
	let is_trailing = utf16_is_trailing;;
	let decode3 = utf16_decode3;;
	let decode = utf16_decode;;
	let encode4 = utf16_encode4;;
	let encode = utf16_encode;;
	include (Bigarray.Array1: BA1_without_t);;
	type t = utf16_string;;
	external compare: t -> t -> int = "%compare";;
	external equal: t -> t -> bool = "%equal";;
	external length: t -> int = "%caml_ba_dim_1";;
	let empty = Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout
		[| |];;
	let create = Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout;;
	let copy = ba_copy;;
	let cat = ba_cat;;
	let fill = ba_fill;;
	let blit = ba_blit;;
	let lead = utf16_lead;;
	let rear = utf16_rear;;
	let get_code = utf16_get_code;;
	let set_code = utf16_set_code;;
	let of_utf8 = utf16_of_utf8;;
	let of_utf32 = utf16_of_utf32;;
	let of_array = Bigarray.Array1.of_array Bigarray.int16_unsigned
		Bigarray.c_layout;;
end;;

module UTF32 = struct
	type elt = utf32_char;;
	let sequence = utf32_sequence;;
	let max_sequence = 1;;
	let is_trailing = utf32_is_trailing;;
	let decode3 = utf32_decode3;;
	let decode = utf32_decode;;
	let encode4 = utf32_encode4;;
	let encode = utf32_encode;;
	include (Bigarray.Array1: BA1_without_t);;
	type t = utf32_string;;
	let compare: t -> t -> int =
		let rec compare x y min_length i = (
			if i >= min_length then Int.compare (dim x) (dim y) else
			let r = Uint32.compare (utf32_unsafe_get x i) (utf32_unsafe_get y i) in
			if r <> 0 then r else
			compare x y min_length (i + 1)
		) in
		fun x y -> compare x y (min (dim x) (dim y)) 0;;
	external equal: t -> t -> bool = "%equal";; (* that compares with signed *)
	external length: t -> int = "%caml_ba_dim_1";;
	let get = utf32_get;;
	let unsafe_get = utf32_unsafe_get;;
	let set (a: t) (x: int) (v: elt) = (
		set a x (Uint32.to_int32 v)
	);;
	let unsafe_set (a: t) (x: int) (v: elt) = (
		unsafe_set a x (Uint32.to_int32 v)
	);;
	let empty = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [| |];;
	let create = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout;;
	let copy = ba_copy;;
	let cat = ba_cat;;
	let fill (a: t) (start: int) (len: int) (c: elt) = (
		ba_fill a start len (Uint32.to_int32 c)
	);;
	let blit = ba_blit;;
	let lead = utf32_lead;;
	let rear = utf32_rear;;
	let get_code = utf32_get_code;;
	let set_code = utf32_set_code;;
	let of_utf8 = utf32_of_utf8;;
	let of_utf16 = utf32_of_utf16;;
	let of_array (source: elt array) = (
		let length = Array.length source in
		let result = create length in
		for i = 0 to length - 1 do
			unsafe_set result i (Uint32.unsafe_get source i)
		done;
		result
	);;
end;;
