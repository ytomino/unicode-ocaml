let slice a start len = (
	if start = 0 && len = Bigarray.Array1.dim a then a else
	Bigarray.Array1.sub a start len
);;

let ba_copy source = (
	let result = Bigarray.Array1.create (Bigarray.Array1.kind source) Bigarray.c_layout (Bigarray.Array1.dim source) in
	Bigarray.Array1.blit source result;
	result
);;

let ba_append x y = (
	let x_length = Bigarray.Array1.dim x in
	let y_length = Bigarray.Array1.dim y in
	let result = Bigarray.Array1.create (Bigarray.Array1.kind x) Bigarray.c_layout (x_length + y_length) in
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

let add_to_bytes index dest code = (
	Bytes.set dest index code;
	index + 1
);;

let add_to_ba index dest code = (
	Bigarray.Array1.set dest index code;
	index + 1
);;

let string_end source index = (
	index >= String.length source
);;

let ba_end source index = (
	index >= Bigarray.Array1.dim source
);;

let succ_snd _: int -> int = succ;;

let finish_get_code _ (b: int) (c: int ref) _ _ (result: Uchar.t): Uchar.t = (
	c := b;
	result
);;

let optional_raise (e: exn option): unit = (
	match e with
	| Some e -> raise e
	| None -> ()
);;

let check_surrogate_pair (illegal_sequence: exn option) (code: int): unit = (
	if code >= 0xd800 && code <= 0xdfff then (
		optional_raise illegal_sequence
	)
);;

let check_range (illegal_sequence: exn option) (code: Int32.t): unit = (
	if code < 0l then (
		optional_raise illegal_sequence
	)
);;

type ucs4 = Int32.t;;

module UCS4 = struct
	include Int32;;
	let of_int x = Int32.logand (Int32.of_int x) 0x7fffffffl;;
	let to_int x = Int32.to_int x land 0x7fffffff;;
end;;

type utf8_char = char;;
type utf8_string = string;;

type utf16_char = int;;
type utf16_string = (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;;

type utf32_char = UCS4.t;;
type utf32_string = (UCS4.t, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;;

let utf8_get: utf8_string -> int -> utf8_char = String.get;;

let utf8_storing_length (code: int): int = (
	if code land 0x7fffff80 = 0 then 1 else
	if code land lnot (1 lsl (5 + 6) - 1) = 0 then 2 else
	if code land lnot (1 lsl (4 + 12) - 1) = 0 then 3 else
	if code land lnot (1 lsl (3 + 18) - 1) = 0 then 4 else
	if code land lnot (1 lsl (2 + 24) - 1) = 0 then 5 else
	6
);;

let utf8_sequence ?(illegal_sequence: exn option) (lead: utf8_char) = (
	let lead = int_of_char lead in
	if lead land 0b10000000 = 0 then (
		1
	) else if lead land 0b11100000 = 0b11000000 then (
		2
	) else if lead land 0b11110000 = 0b11100000 then (
		3
	) else if lead land 0b11111000 = 0b11110000 then (
		4
	) else if lead land 0b11111100 = 0b11111000 then (
		5
	) else if lead land 0b11111110 = 0b11111100 then (
		6
	) else (
		optional_raise illegal_sequence;
		1
	)
);;

let utf8_decode ?(illegal_sequence: exn option) (get_f: 'a -> 'b -> utf8_char) (inc_f: 'a -> 'b -> 'b) (end_f: 'a -> 'b -> bool) a b c d e (cont: 'a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) = (
	let rec tails illegal_sequence get_f inc_f end_f a b c d e cont length offset code = (
		if offset > 0 then (
			if not (end_f a b) then (
				let tail = int_of_char (get_f a b) in
				if tail >= 0b10000000 && tail < 0b10111111 then (
					let b = inc_f a b in
					tails illegal_sequence get_f inc_f end_f a b c d e cont length (offset - 6) (code lsl 6 lor (tail land 0b00111111))
				) else (
					(* trailing is illegal *)
					optional_raise illegal_sequence;
					tails illegal_sequence get_f inc_f end_f a b c d e cont length 0 (code lsl offset)
				)
			) else (
				(* no trailing *)
				optional_raise illegal_sequence;
				tails illegal_sequence get_f inc_f end_f a b c d e cont length 0 (code lsl offset)
			)
		) else (
			let result = code in
			check_surrogate_pair illegal_sequence result;
			if utf8_storing_length result <> length then (
				optional_raise illegal_sequence
			);
			cont a b c d e (Uchar.unsafe_of_int result)
		)
	) in
	let lead = int_of_char (get_f a b) in
	let b = inc_f a b in
	if lead land 0b10000000 = 0 then (
		cont a b c d e (Uchar.unsafe_of_int lead)
	) else if lead land 0b11100000 = 0b11000000 then (
		tails illegal_sequence get_f inc_f end_f a b c d e cont 2 6 (lead land 0b00011111)
	) else if lead land 0b11110000 = 0b11100000 then (
		tails illegal_sequence get_f inc_f end_f a b c d e cont 3 12 (lead land 0b00001111)
	) else if lead land 0b11111000 = 0b11110000 then (
		tails illegal_sequence get_f inc_f end_f a b c d e cont 4 18 (lead land 0b00000111)
	) else if lead land 0b11111100 = 0b11111000 then (
		tails illegal_sequence get_f inc_f end_f a b c d e cont 5 24 (lead land 0b00000011)
	) else if lead land 0b11111110 = 0b11111100 then (
		tails illegal_sequence get_f inc_f end_f a b c d e cont 6 30 (lead land 0b00000001)
	) else (
		optional_raise illegal_sequence;
		let fake = lead + 0x7fffff00 in (* out of meaning code-points, by way of precaution *)
		cont a b c d e (Uchar.unsafe_of_int fake)
	)
);;

let utf8_get_code ?(illegal_sequence: exn option) (source: utf8_string) (index: int ref): Uchar.t = (
	utf8_decode ?illegal_sequence utf8_get succ_snd string_end source !index index () () finish_get_code
);;

let utf8_lead (s: utf8_string) (i: int): int = (
	let rec lead s i = (
		if i = 0 || int_of_char (String.get s i) land 0b11000000 <> 0b10000000 then i else
		lead s (i - 1)
	) in
	let first = lead s i in
	if first + utf8_sequence (String.get s first) > i then first else i
);;

let utf8_encode ?(illegal_sequence: exn option) (f: 'a -> 'b -> utf8_char -> 'a) a b (code: Uchar.t) = (
	ignore illegal_sequence; (* without checking surrogate pair in set *)
	let rec tails f offset a b code = (
		if offset <= 0 then a else (
			let offset = offset - 6 in
			let a = f a b (char_of_int (code lsr offset land 0b00111111 lor 0b10000000)) in
			tails f offset a b code
		)
	) in
	let code = Uchar.to_int code in
	let length = utf8_storing_length code in
	if length = 1 then (
		f a b (char_of_int code)
	) else (
		let offset = (length - 1) * 6 in
		let leading = char_of_int (code lsr offset lor (0xff lxor (1 lsl (8 - length) - 1))) in
		let a = f a b leading in
		tails f offset a b code
	)
);;

let utf8_set_code ?(illegal_sequence: exn option) (dest: bytes) (index: int ref) (code: Uchar.t): unit = (
	index := utf8_encode ?illegal_sequence add_to_bytes !index dest code
);;

let utf16_get: utf16_string -> int -> utf16_char = Bigarray.Array1.get;;

let utf16_sequence ?(illegal_sequence: exn option) (lead: utf16_char) = (
	if lead >= 0 && lead <= 0xd7ff || lead >= 0xe000 && lead <= 0xffff then (
		1
	) else if lead >= 0xd800 && lead <= 0xdbff then (
		2
	) else (
		optional_raise illegal_sequence;
		1
	)
);;

let utf16_decode ?(illegal_sequence: exn option) (get_f: 'a -> 'b -> utf16_char) (inc_f: 'a -> 'b -> 'b) (end_f: 'a -> 'b -> bool) a b c d e (cont: 'a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) = (
	let lead = get_f a b in
	let b = inc_f a b in
	if lead >= 0 && lead <= 0xd7ff || lead >= 0xe000 && lead <= 0xffff then (
		cont a b c d e (Uchar.unsafe_of_int lead)
	) else if lead >= 0xd800 && lead <= 0xdbff then (
		let tail, b =
			if end_f a b then (
				(* leading, but no trailing *)
				optional_raise illegal_sequence;
				0, b
			) else (
				let tail = get_f a b in
				if tail < 0xdc00 || tail > 0xdfff then (
					(* leading, but trailing is illegal *)
					optional_raise illegal_sequence;
					0, b
				) else (
					let b = inc_f a b in
					tail, b
				)
			)
		in
		let result = ((lead land (1 lsl 10 - 1)) lsl 10 lor (tail land (1 lsl 10 - 1))) + 0x10000 in
		cont a b c d e (Uchar.unsafe_of_int result)
	) else (
		(* illegal *)
		optional_raise illegal_sequence;
		cont a b c d e (Uchar.unsafe_of_int lead)
	)
);;

let utf16_get_code ?(illegal_sequence: exn option) (source: utf16_string) (index: int ref): Uchar.t = (
	utf16_decode ?illegal_sequence utf16_get succ_snd ba_end source !index index () () finish_get_code
);;

let utf16_lead (s: utf16_string) (i: int): int = (
	if i > 0 &&
		let c = Bigarray.Array1.get s i in
		c >= 0xdc00 && c <= 0xdfff &&
		let p = Bigarray.Array1.unsafe_get s (i - 1) in
		p >= 0xd800 && p <= 0xdbff
	then i - 1 else i
);;

let utf16_encode ?(illegal_sequence: exn option) (f: 'a -> 'b -> utf16_char -> 'a) a b (code: Uchar.t) = (
	(* without checking surrogate pair in set *)
	let code = Uchar.to_int code in
	if code <= 0xffff then (
		f a b code
	) else (
		let c2 =
			if code >= 0x110000 then (
				optional_raise illegal_sequence;
				0xfffff
			) else (
				code - 0x10000
			)
		in
		let a = f a b (0xd800 lor ((c2 lsr 10) land (1 lsl 10 - 1))) in
		f a b (0xdc00 lor (c2 land (1 lsl 10 - 1)))
	)
);;

let utf16_set_code ?(illegal_sequence: exn option) (dest: utf16_string) (index: int ref) (code: Uchar.t): unit = (
	index := utf16_encode ?illegal_sequence add_to_ba !index dest code
);;

let utf32_get: utf32_string -> int -> utf32_char = Bigarray.Array1.get;;

let utf32_sequence ?(illegal_sequence: exn option) (lead: utf32_char): int = (
	check_range illegal_sequence lead;
	check_surrogate_pair illegal_sequence (UCS4.to_int lead);
	1
);;

let utf32_decode ?(illegal_sequence: exn option) (get_f: 'a -> 'b -> utf32_char) (inc_f: 'a -> 'b -> 'b) (end_f: 'a -> 'b -> bool) a b c d e (cont: 'a -> 'b -> 'c -> 'd -> 'e -> Uchar.t -> 'f) = (
	ignore end_f;
	let result = get_f a b in
	check_range illegal_sequence result;
	let result = UCS4.to_int result in
	check_surrogate_pair illegal_sequence result;
	let b = inc_f a b in
	cont a b c d e (Uchar.unsafe_of_int result)
);;

let utf32_get_code ?(illegal_sequence: exn option) (source: utf32_string) (index: int ref): Uchar.t = (
	utf32_decode ?illegal_sequence utf32_get succ_snd ba_end source !index index () () finish_get_code
);;

let utf32_lead (_: utf32_string) (i: int): int = i;;

let utf32_encode ?(illegal_sequence: exn option) (f: 'a -> 'b -> utf32_char -> 'a) a b (code: Uchar.t) = (
	ignore illegal_sequence; (* without checking surrogate pair in set *)
	let code = Uchar.to_int code in
	f a b (UCS4.of_int code)
);;

let utf32_set_code ?(illegal_sequence: exn option) (dest: utf32_string) (index: int ref) (code: Uchar.t): unit = (
	index := utf32_encode ?illegal_sequence add_to_ba !index dest code
);;

let utf8_of_utf16 ?(illegal_sequence: exn option) (source: utf16_string): utf8_string = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bytes.create (3 * source_length) in
	let rec make illegal_sequence source i result j = (
		if i >= Bigarray.Array1.dim source then Bytes.unsafe_to_string (Bytes.sub result 0 j) else (
			utf16_decode ?illegal_sequence utf16_get succ_snd ba_end source i result j illegal_sequence (fun source i result j illegal_sequence code ->
				let j = utf8_encode ?illegal_sequence add_to_bytes j result code in
				make illegal_sequence source i result j
			)
		)
	) in
	make illegal_sequence source 0 result 0
);;

let utf8_of_utf32 ?(illegal_sequence: exn option) (source: utf32_string): utf8_string = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bytes.create (6 * source_length) in
	let rec make illegal_sequence source i result j = (
		if i >= Bigarray.Array1.dim source then Bytes.unsafe_to_string (Bytes.sub result 0 j) else (
			utf32_decode ?illegal_sequence utf32_get succ_snd ba_end source i result j illegal_sequence (fun source i result j illegal_sequence code ->
				let j = utf8_encode ?illegal_sequence add_to_bytes j result code in
				make illegal_sequence source i result j
			)
		)
	) in
	make illegal_sequence source 0 result 0
);;

let utf16_of_utf8 ?(illegal_sequence: exn option) (source: utf8_string): utf16_string = (
	let source_length = String.length source in
	let result = Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout source_length in
	let rec make illegal_sequence source i result j = (
		if i >= String.length source then slice result 0 j else (
			utf8_decode ?illegal_sequence utf8_get succ_snd string_end source i result j illegal_sequence (fun source i result j illegal_sequence code ->
				let j = utf16_encode ?illegal_sequence add_to_ba j result code in
				make illegal_sequence source i result j
			)
		)
	) in
	make illegal_sequence source 0 result 0
);;

let utf16_of_utf32 ?(illegal_sequence: exn option) (source: utf32_string): utf16_string = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout (2 * source_length) in
	let rec make illegal_sequence source i result j = (
		if i >= Bigarray.Array1.dim source then Bigarray.Array1.sub result 0 j else (
			utf32_decode ?illegal_sequence utf32_get succ_snd ba_end source i result j illegal_sequence (fun source i result j illegal_sequence code ->
				let j = utf16_encode ?illegal_sequence add_to_ba j result code in
				make illegal_sequence source i result j
			)
		)
	) in
	make illegal_sequence source 0 result 0
);;

let utf32_of_utf8 ?(illegal_sequence: exn option) (source: utf8_string): utf32_string = (
	let source_length = String.length source in
	let result = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout source_length in
	let rec make illegal_sequence source i result j = (
		if i >= String.length source then slice result 0 j else (
			utf8_decode ?illegal_sequence utf8_get succ_snd string_end source i result j illegal_sequence (fun source i result j illegal_sequence code ->
				let j = utf32_encode ?illegal_sequence add_to_ba j result code in
				make illegal_sequence source i result j
			)
		)
	) in
	make illegal_sequence source 0 result 0
);;

let utf32_of_utf16 ?(illegal_sequence: exn option) (source: utf16_string): utf32_string = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout source_length in
	let rec make illegal_sequence source i result j = (
		if i >= Bigarray.Array1.dim source then slice result 0 j else (
			utf16_decode ?illegal_sequence utf16_get succ_snd ba_end source i result j illegal_sequence (fun source i result j illegal_sequence code ->
				let j = utf32_encode ?illegal_sequence add_to_ba j result code in
				make illegal_sequence source i result j
			)
		)
	) in
	make illegal_sequence source 0 result 0
);;

module UTF8 = struct
	type elm = utf8_char;;
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
	let of_array (source: elm array): t = (
		let length = Array.length source in
		let result = Bytes.create length in
		for i = 0 to length - 1 do
			Bytes.unsafe_set result i (Array.unsafe_get source i)
		done;
		Bytes.unsafe_to_string result
	);;
end;;

module type BA1_without_t = module type of Bigarray.Array1 with type ('a, 'b, 'c) t := ('a, 'b, 'c) Bigarray.Array1.t;;

module UTF16 = struct
	include (struct include Bigarray.Array1 end: BA1_without_t);;
	type elm = utf16_char;;
	type t = utf16_string;;
	external compare: t -> t -> int = "%compare";;
	external length: t -> int = "%caml_ba_dim_1";;
	let empty = Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout [| |];;
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
	let of_array = Bigarray.Array1.of_array Bigarray.int16_unsigned Bigarray.c_layout;;
end;;

module UTF32 = struct
	include (struct include Bigarray.Array1 end: BA1_without_t);;
	type elm = utf32_char;;
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
