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

let ba_append x y = (
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

let string_end source index = (
	index >= String.length source
);;

let ba_end source index = (
	index >= Bigarray.Array1.dim source
);;

let succ_snd _: int -> int = succ;;

let finish_get_code _ _ (c: int ref) _ (e: int) (result: Uchar.t) = (
	c := e;
	result
);;

let optional_raise (e: exn option) = (
	match e with
	| Some e -> raise e
	| None -> ()
);;

let check_surrogate_pair (illegal_sequence: exn option) (code: int) = (
	if code >= 0xd800 && code <= 0xdfff then (
		optional_raise illegal_sequence
	)
);;

module type Uint32_S = sig
	type t [@@ocaml.immediate64]
	val compare: t -> t -> int
	val is_uint31: t -> bool
	val of_int: int -> t
	val of_int32: int32 -> t
	val to_int: t -> int
	val to_int32: t -> int32
end;;

module Non_immediate_Uint32 = struct
	type t = int32;;
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
end;;

module Immediate_Uint32 = struct
	type t = int;;
	let compare = Int.compare;;
	let is_uint31 x = x land lnot 0x7fffffff = 0;;
	let of_int x = x land (1 lsl 32 - 1);;
	let of_int32 x = Int32.to_int x land (1 lsl 32 - 1);;
	let to_int x = x;;
	let to_int32 = Int32.of_int;;
end;;

module Uint32 =
	(val
		if Sys.word_size <= 32 then (module Non_immediate_Uint32: Uint32_S) else
		(module Immediate_Uint32: Uint32_S)
	);;

type utf8_char = char;;
type utf8_string = string;;

type utf16_char = int;;
type utf16_string =
	(int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;;

type utf32_char = Uint32.t;;
type utf32_string =
	(int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;;

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

let utf8_sequence ?(illegal_sequence: exn option) (lead: utf8_char) = (
	let lead = int_of_char lead in
	if lead land 0b10000000 = 0 then 1 else
	if lead land 0b11100000 = 0b11000000 then 2 else
	if lead land 0b11110000 = 0b11100000 then 3 else
	if lead land 0b11111000 = 0b11110000 then 4 else
	if lead land 0b11111100 = 0b11111000 then 5 else
	if lead land 0b11111110 = 0b11111100 then 6 else (
		optional_raise illegal_sequence;
		1
	)
);;

let utf8_decode ?(illegal_sequence: exn option) (get_f: 'd -> 'e -> utf8_char)
	(inc_f: 'd -> 'e -> 'e) (end_f: 'd -> 'e -> bool) a b c d e
	(cont: 'a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) =
(
	let finish illegal_sequence a b c d e cont length code = (
			check_surrogate_pair illegal_sequence code;
			if utf8_storing_length code <> length then (
				optional_raise illegal_sequence
			);
			cont a b c d e (Uchar.unsafe_of_int code)
	) in
	let rec tails illegal_sequence get_f inc_f end_f a b c d e cont length offset
		code =
	(
		if offset <= 0 then finish illegal_sequence a b c d e cont length code else (
			if end_f d e then (
				(* no trailing *)
				optional_raise illegal_sequence;
				finish illegal_sequence a b c d e cont length (code lsl offset)
			) else (
				let tail = int_of_char (get_f d e) in
				if tail < 0b10000000 || tail > 0b10111111 then (
					(* trailing is illegal *)
					optional_raise illegal_sequence;
					finish illegal_sequence a b c d e cont length (code lsl offset)
				) else (
					let e = inc_f d e in
					tails illegal_sequence get_f inc_f end_f a b c d e cont length (offset - 6)
						(code lsl 6 lor (tail land 0b00111111))
				)
			)
		)
	) in
	let lead = int_of_char (get_f d e) in
	let e = inc_f d e in
	if lead land 0b10000000 = 0 then (
		cont a b c d e (Uchar.unsafe_of_int lead)
	) else if lead land 0b11100000 = 0b11000000 then (
		tails illegal_sequence get_f inc_f end_f a b c d e cont 2 6
			(lead land 0b00011111)
	) else if lead land 0b11110000 = 0b11100000 then (
		tails illegal_sequence get_f inc_f end_f a b c d e cont 3 12
			(lead land 0b00001111)
	) else if lead land 0b11111000 = 0b11110000 then (
		tails illegal_sequence get_f inc_f end_f a b c d e cont 4 18
			(lead land 0b00000111)
	) else if lead land 0b11111100 = 0b11111000 then (
		tails illegal_sequence get_f inc_f end_f a b c d e cont 5 24
			(lead land 0b00000011)
	) else if lead land 0b11111110 = 0b11111100 then (
		tails illegal_sequence get_f inc_f end_f a b c d e cont 6 30
			(lead land 0b00000001)
	) else (
		optional_raise illegal_sequence;
		let fake = lead + 0x7fffff00 in
			(* out of meaning code-points, by way of precaution *)
		cont a b c d e (Uchar.unsafe_of_int fake)
	)
);;

let utf8_get_code ?(illegal_sequence: exn option) (source: utf8_string)
	(index: int ref) =
(
	utf8_decode ?illegal_sequence utf8_get succ_snd string_end () () index source
		!index finish_get_code
);;

let utf8_lead (s: utf8_string) (i: int) = (
	let rec lead s i j c = (
		if j <= 0 || int_of_char c land 0b11000000 <> 0b10000000 then (
			if j + utf8_sequence (c) <= i then i else
			j
		) else
		let n = j - 1 in
		lead s i n (String.unsafe_get s n)
	) in
	lead s i i (String.get s i)
);;

let utf8_encode ?(illegal_sequence: exn option)
	(f: 'a -> 'b -> utf8_char -> 'b) a b (code: Uchar.t) =
(
	ignore illegal_sequence; (* without checking surrogate pair in set *)
	let rec tails f offset a b code = (
		if offset <= 0 then b else (
			let offset = offset - 6 in
			let b = f a b (char_of_int (code lsr offset land 0b00111111 lor 0b10000000)) in
			tails f offset a b code
		)
	) in
	let code = Uchar.to_int code in
	let length = utf8_storing_length code in
	if length = 1 then f a b (char_of_int code) else (
		let offset = (length - 1) * 6 in
		let mask = (1 lsl (8 - length) - 1) in
		let leading = char_of_int
			(code lsr offset land (mask lsr 1) lor (0xff lxor mask))
		in
		let b = f a b leading in
		tails f offset a b code
	)
);;

let utf8_set_code ?(illegal_sequence: exn option) (dest: bytes)
	(index: int ref) (code: Uchar.t) =
(
	index := utf8_encode ?illegal_sequence utf8_add dest !index code
);;

let utf16_get: utf16_string -> int -> utf16_char = Bigarray.Array1.get;;

let utf16_add (dest: utf16_string) (index: int) (item: utf16_char) = (
	Bigarray.Array1.set dest index item;
	index + 1
);;

let utf16_sequence ?(illegal_sequence: exn option) (lead: utf16_char) = (
	if lead >= 0 && lead <= 0xd7ff || lead >= 0xe000 && lead <= 0xffff then 1 else
	if lead >= 0xd800 && lead <= 0xdbff then 2 else (
		optional_raise illegal_sequence;
		1
	)
);;

let utf16_decode ?(illegal_sequence: exn option)
	(get_f: 'd -> 'e -> utf16_char) (inc_f: 'd -> 'e -> 'e)
	(end_f: 'd -> 'e -> bool) a b c d e
	(cont: 'a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) =
(
	let lead = get_f d e in
	let e = inc_f d e in
	if lead >= 0 && lead <= 0xd7ff || lead >= 0xe000 && lead <= 0xffff then (
		cont a b c d e (Uchar.unsafe_of_int lead)
	) else if lead >= 0xd800 && lead <= 0xdbff then (
		let tail, e =
			if end_f d e then (
				(* leading, but no trailing *)
				optional_raise illegal_sequence;
				0, e
			) else (
				let tail = get_f d e in
				if tail < 0xdc00 || tail > 0xdfff then (
					(* leading, but trailing is illegal *)
					optional_raise illegal_sequence;
					0, e
				) else (
					let e = inc_f d e in
					tail, e
				)
			)
		in
		let result = ((lead land (1 lsl 10 - 1)) lsl 10 lor (tail land (1 lsl 10 - 1)))
			+ 0x10000
		in
		cont a b c d e (Uchar.unsafe_of_int result)
	) else (
		(* illegal *)
		optional_raise illegal_sequence;
		cont a b c d e (Uchar.unsafe_of_int lead)
	)
);;

let utf16_get_code ?(illegal_sequence: exn option) (source: utf16_string)
	(index: int ref) =
(
	utf16_decode ?illegal_sequence utf16_get succ_snd ba_end () () index source
		!index finish_get_code
);;

let utf16_lead (s: utf16_string) (i: int) = (
	let c = Bigarray.Array1.get s i in
	if i <= 0 || c < 0xdc00 || c > 0xdfff then i else
	let n = i - 1 in
	let p = Bigarray.Array1.unsafe_get s n in
	if p < 0xd800 || p > 0xdbff then i else
	n
);;

let utf16_encode ?(illegal_sequence: exn option)
	(f: 'a -> 'b -> utf16_char -> 'b) a b (code: Uchar.t) =
(
	(* without checking surrogate pair in set *)
	let code = Uchar.to_int code in
	if code land lnot 0xffff = 0 then f a b code else (
		let c2 =
			let c2 = code - 0x10000 in
			if code land lnot 0xfffff <> 0 then (
				optional_raise illegal_sequence;
				0xfffff
			) else (
				c2
			)
		in
		let b = f a b (0xd800 lor ((c2 lsr 10) land (1 lsl 10 - 1))) in
		f a b (0xdc00 lor (c2 land (1 lsl 10 - 1)))
	)
);;

let utf16_set_code ?(illegal_sequence: exn option) (dest: utf16_string)
	(index: int ref) (code: Uchar.t) =
(
	index := utf16_encode ?illegal_sequence utf16_add dest !index code
);;

let check_range (illegal_sequence: exn option) (item: utf32_char) = (
	if not (Uint32.is_uint31 item) then (
		optional_raise illegal_sequence
	)
);;

let utf32_get (source: utf32_string) (index: int) = (
	Uint32.of_int32 (Bigarray.Array1.get source index)
);;

let utf32_add (dest: utf32_string) (index: int) (item: utf32_char) = (
	Bigarray.Array1.set dest index (Uint32.to_int32 item);
	index + 1
);;

let utf32_sequence ?(illegal_sequence: exn option) (lead: utf32_char) = (
	check_range illegal_sequence lead;
	let lead = Uint32.to_int lead in
	check_surrogate_pair illegal_sequence lead;
	1
);;

let utf32_decode ?(illegal_sequence: exn option)
	(get_f: 'd -> 'e -> utf32_char) (inc_f: 'd -> 'e -> 'e)
	(end_f: 'd -> 'e -> bool) a b c d e
	(cont: 'a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) =
(
	ignore end_f;
	let result = get_f d e in
	check_range illegal_sequence result;
	let result = Uint32.to_int result in
	check_surrogate_pair illegal_sequence result;
	let e = inc_f d e in
	cont a b c d e (Uchar.unsafe_of_int result)
);;

let utf32_get_code ?(illegal_sequence: exn option) (source: utf32_string)
	(index: int ref) =
(
	utf32_decode ?illegal_sequence utf32_get succ_snd ba_end () () index source
		!index finish_get_code
);;

let utf32_lead (_: utf32_string) (i: int) = i;;

let utf32_encode ?(illegal_sequence: exn option)
	(f: 'a -> 'b -> utf32_char -> 'b) a b (code: Uchar.t) =
(
	ignore illegal_sequence; (* without checking surrogate pair in set *)
	let code = Uchar.to_int code in
	f a b (Uint32.of_int code)
);;

let utf32_set_code ?(illegal_sequence: exn option) (dest: utf32_string)
	(index: int ref) (code: Uchar.t) =
(
	index := utf32_encode ?illegal_sequence utf32_add dest !index code
);;

let utf8_of_utf16 ?(illegal_sequence: exn option) (source: utf16_string) = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bytes.create (3 * source_length) in
	let rec make illegal_sequence result j source i = (
		if i >= Bigarray.Array1.dim source
		then Bytes.unsafe_to_string (Bytes.sub result 0 j) else
		utf16_decode ?illegal_sequence utf16_get succ_snd ba_end illegal_sequence
			result j source i (fun illegal_sequence result j source i code ->
				let j = utf8_encode ?illegal_sequence utf8_add result j code in
				make illegal_sequence result j source i
			)
	) in
	make illegal_sequence result 0 source 0
);;

let utf8_of_utf32 ?(illegal_sequence: exn option) (source: utf32_string) = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bytes.create (6 * source_length) in
	let rec make illegal_sequence result j source i = (
		if i >= Bigarray.Array1.dim source
		then Bytes.unsafe_to_string (Bytes.sub result 0 j) else
		utf32_decode ?illegal_sequence utf32_get succ_snd ba_end illegal_sequence
			result j source i (fun illegal_sequence result j source i code ->
				let j = utf8_encode ?illegal_sequence utf8_add result j code in
				make illegal_sequence result j source i
			)
	) in
	make illegal_sequence result 0 source 0
);;

let utf16_of_utf8 ?(illegal_sequence: exn option) (source: utf8_string) = (
	let source_length = String.length source in
	let result = Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout
		source_length
	in
	let rec make illegal_sequence result j source i = (
		if i >= String.length source then slice result 0 j else
		utf8_decode ?illegal_sequence utf8_get succ_snd string_end illegal_sequence
			result j source i (fun illegal_sequence result j source i code ->
				let j = utf16_encode ?illegal_sequence utf16_add result j code in
				make illegal_sequence result j source i
			)
	) in
	make illegal_sequence result 0 source 0
);;

let utf16_of_utf32 ?(illegal_sequence: exn option) (source: utf32_string) = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout
		(2 * source_length)
	in
	let rec make illegal_sequence result j source i = (
		if i >= Bigarray.Array1.dim source then Bigarray.Array1.sub result 0 j else
		utf32_decode ?illegal_sequence utf32_get succ_snd ba_end illegal_sequence
			result j source i (fun illegal_sequence result j source i code ->
				let j = utf16_encode ?illegal_sequence utf16_add result j code in
				make illegal_sequence result j source i
			)
	) in
	make illegal_sequence result 0 source 0
);;

let utf32_of_utf8 ?(illegal_sequence: exn option) (source: utf8_string) = (
	let source_length = String.length source in
	let result = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout
		source_length
	in
	let rec make illegal_sequence result j source i = (
		if i >= String.length source then slice result 0 j else
		utf8_decode ?illegal_sequence utf8_get succ_snd string_end illegal_sequence
			result j source i (fun illegal_sequence result j source i code ->
				let j = utf32_encode ?illegal_sequence utf32_add result j code in
				make illegal_sequence result j source i
			)
	) in
	make illegal_sequence result 0 source 0
);;

let utf32_of_utf16 ?(illegal_sequence: exn option) (source: utf16_string) = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout
		source_length
	in
	let rec make illegal_sequence result j source i = (
		if i >= Bigarray.Array1.dim source then slice result 0 j else
		utf16_decode ?illegal_sequence utf16_get succ_snd ba_end illegal_sequence
			result j source i (fun illegal_sequence result j source i code ->
				let j = utf32_encode ?illegal_sequence utf32_add result j code in
				make illegal_sequence result j source i
			)
	) in
	make illegal_sequence result 0 source 0
);;

module UTF8 = struct
	type elt = utf8_char;;
	include String;;
	let empty = "";;
	let append = ( ^ );;
	let sequence = utf8_sequence;;
	let max_sequence = 6;;
	let decode = utf8_decode;;
	let get_code = utf8_get_code;;
	let lead = utf8_lead;;
	let encode = utf8_encode;;
	let set_code = utf8_set_code;;
	let of_utf16 = utf8_of_utf16;;
	let of_utf32 = utf8_of_utf32;;
	let of_array (source: elt array) = (
		let length = Array.length source in
		let result = Bytes.create length in
		for i = 0 to length - 1 do
			Bytes.unsafe_set result i (Array.unsafe_get source i)
		done;
		Bytes.unsafe_to_string result
	);;
end;;

module type BA1_without_t = module type of Bigarray.Array1
	with type ('a, 'b, 'c) t := ('a, 'b, 'c) Bigarray.Array1.t;;

module UTF16 = struct
	include (struct include Bigarray.Array1 end: BA1_without_t);;
	type elt = utf16_char;;
	type t = utf16_string;;
	external compare: t -> t -> int = "%compare";;
	external length: t -> int = "%caml_ba_dim_1";;
	let empty = Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout
		[| |];;
	let create = Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout;;
	let copy = ba_copy;;
	let append = ba_append;;
	let fill = ba_fill;;
	let blit = ba_blit;;
	let sequence = utf16_sequence;;
	let max_sequence = 2;;
	let decode = utf16_decode;;
	let get_code = utf16_get_code;;
	let lead = utf16_lead;;
	let encode = utf16_encode;;
	let set_code = utf16_set_code;;
	let of_utf8 = utf16_of_utf8;;
	let of_utf32 = utf16_of_utf32;;
	let of_array = Bigarray.Array1.of_array Bigarray.int16_unsigned
		Bigarray.c_layout;;
end;;

module UTF32 = struct
	include (struct include Bigarray.Array1 end: BA1_without_t);;
	type elt = int32;;
	type t = utf32_string;;
	external compare: t -> t -> int = "%compare";;
	external length: t -> int = "%caml_ba_dim_1";;
	let empty = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout [| |];;
	let create = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout;;
	let copy = ba_copy;;
	let append = ba_append;;
	let fill = ba_fill;;
	let blit = ba_blit;;
	let sequence = utf32_sequence;;
	let max_sequence = 1;;
	let decode = utf32_decode;;
	let get_code = utf32_get_code;;
	let lead = utf32_lead;;
	let encode = utf32_encode;;
	let set_code = utf32_set_code;;
	let of_utf8 = utf32_of_utf8;;
	let of_utf16 = utf32_of_utf16;;
	let of_array = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout;;
end;;
