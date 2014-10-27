let ba_copy source = (
	let result = Bigarray.Array1.create (Bigarray.Array1.kind source) Bigarray.c_layout (Bigarray.Array1.dim source) in
	Bigarray.Array1.blit source result;
	result
);;

let ba_append x y = (
	let x_length = Bigarray.Array1.dim x in
	let y_length = Bigarray.Array1.dim y in
	let result = Bigarray.Array1.create (Bigarray.Array1.kind x) Bigarray.c_layout (x_length + y_length) in
	Bigarray.Array1.blit x (Bigarray.Array1.sub result 0 x_length);
	Bigarray.Array1.blit y (Bigarray.Array1.sub result x_length y_length);
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

type ucs4 = Int32.t;;

module UCS4 = struct
	include Int32;;
	let of_int x = Int32.logand (Int32.of_int x) 0x7fffffffl;;
end;;

type utf8_char = char;;
type utf8_string = string;;

type utf16_char = int;;
type utf16_string = (int, Bigarray.int16_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;;

type utf32_char = UCS4.t;;
type utf32_string = (UCS4.t, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;;

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

let utf8_get_code ?(illegal_sequence: exn option) (source: utf8_string) (index: int ref): int = (
	let tails1 illegal_sequence length source index code = (
		let rec tails2 illegal_sequence length source index code = (
			if length <= 0 then code else (
				let tail = int_of_char (String.unsafe_get source !index) in
				if tail >= 0b10000000 && tail < 0b10111111 then (
					incr index;
					tails2 illegal_sequence (pred length) source index (code lsl 6 lor (tail land 0b00111111))
				) else (
					(* trailing is illegal *)
					optional_raise illegal_sequence;
					code lsl (6 * length)
				)
			)
		) in
		let result =
			let source_length = String.length source in
			let sufficient =
				let sufficient = (!index + length) - source_length in
				if sufficient > 0 then (
					optional_raise illegal_sequence;
					sufficient
				) else (
					0
				)
			in
			incr index;
			let r = tails2 illegal_sequence (pred length - sufficient) source index code in
			r lsl (6 * sufficient)
		in
		check_surrogate_pair illegal_sequence result;
		if utf8_storing_length result <> length then (
			optional_raise illegal_sequence
		);
		result
	) in
	let lead = int_of_char (String.get source !index) in
	if lead land 0b10000000 = 0 then (
		incr index;
		lead
	) else if lead land 0b11100000 = 0b11000000 then (
		tails1 illegal_sequence 2 source index (lead land 0b00011111)
	) else if lead land 0b11110000 = 0b11100000 then (
		tails1 illegal_sequence 3 source index (lead land 0b00001111)
	) else if lead land 0b11111000 = 0b11110000 then (
		tails1 illegal_sequence 4 source index (lead land 0b00000111)
	) else if lead land 0b11111100 = 0b11111000 then (
		tails1 illegal_sequence 5 source index (lead land 0b00000011)
	) else if lead land 0b11111110 = 0b11111100 then (
		tails1 illegal_sequence 6 source index (lead land 0b00000001)
	) else (
		optional_raise illegal_sequence;
		incr index;
		lead + 0x7fffff00 (* out of meaning code-points, by way of precaution *)
	)
);;

let utf8_set_code ?(illegal_sequence: exn option) (dest: utf8_string) (index: int ref) (code: int): unit = (
	ignore illegal_sequence; (* without checking surrogate pair in set *)
	let rec tails length dest index code = (
		if length <= 0 then () else (
			Bytes.set (Bytes.unsafe_of_string dest) index (char_of_int (code land 0b00111111 lor 0b10000000));
			tails (pred length) dest (pred index) (code lsr 6)
		)
	) in
	let length = utf8_storing_length code in
	if length = 1 then (
		Bytes.set (Bytes.unsafe_of_string dest) !index (char_of_int code);
		incr index
	) else (
		let length_m_1 = length - 1 in
		let leading = char_of_int (code lsr (length_m_1 * 6) lor (0xff lxor (1 lsl (8 - length) - 1))) in
		Bytes.set (Bytes.unsafe_of_string dest) !index leading;
		index := !index + length;
		tails length_m_1 dest (!index - 1) code
	)
);;

let utf8_lead (s: utf8_string) (i: int): int = (
	let rec lead s i = (
		if i = 0 || int_of_char (String.get s i) land 0b11000000 <> 0b10000000 then i else
		lead s (i - 1)
	) in
	let first = lead s i in
	if first + utf8_sequence (String.get s first) > i then first else i
);;

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

let utf16_get_code ?(illegal_sequence: exn option) (source: utf16_string) (index: int ref): int = (
	let lead = Bigarray.Array1.get source !index in
	if lead >= 0 && lead <= 0xd7ff || lead >= 0xe000 && lead <= 0xffff then (
		incr index;
		lead
	) else if lead >= 0xd800 && lead <= 0xdbff then (
		let tail =
			if Bigarray.Array1.dim source <= !index + 1 then (
				(* leading, but no trailing *)
				optional_raise illegal_sequence;
				incr index;
				0
			) else (
				let tail = Bigarray.Array1.unsafe_get source (!index + 1) in
				if tail < 0xdc00 || tail > 0xdfff then (
					(* leading, but trailing is illegal *)
					optional_raise illegal_sequence;
					incr index;
					0
				) else (
					index := !index + 2;
					tail
				)
			)
		in
		((lead land (1 lsl 10 - 1)) lsl 10 lor (tail land (1 lsl 10 - 1))) + 0x10000
	) else (
		(* illegal *)
		optional_raise illegal_sequence;
		incr index;
		lead
	)
);;

let utf16_set_code ?(illegal_sequence: exn option) (dest: utf16_string) (index: int ref) (code: int): unit = (
	(* without checking surrogate pair in set *)
	if code >= 0 && (* code <= 0xd7ff || code >= 0xe000 && *) code <= 0xffff then (
		Bigarray.Array1.set dest !index code;
		incr index
	) else (
		let c2 = code - 0x10000 in
		if c2 >= 1 lsl 20 then (
			optional_raise illegal_sequence
		);
		Bigarray.Array1.set dest !index (0xd800 lor ((c2 lsr 10) land (1 lsl 10 - 1)));
		Bigarray.Array1.set dest (!index + 1) (0xdc00 lor (c2 land (1 lsl 10 - 1)));
		index := !index + 2
	)
);;

let utf16_lead (s: utf16_string) (i: int): int = (
	if i > 0 &&
		let c = Bigarray.Array1.get s i in
		c >= 0xdc00 && c <= 0xdfff &&
		let p = Bigarray.Array1.unsafe_get s i - 1 in
		p >= 0xd800 && p <= 0xdbff
	then i - 1 else i
);;

let utf32_sequence ?(illegal_sequence: exn option) (lead: utf32_char): int = (
	check_surrogate_pair illegal_sequence (UCS4.to_int lead);
	1
);;

let utf32_get_code ?(illegal_sequence: exn option) (source: utf32_string) (index: int ref): int = (
	let result = Bigarray.Array1.get source !index in
	check_surrogate_pair illegal_sequence (UCS4.to_int result);
	incr index;
	UCS4.to_int result
);;

let utf32_set_code ?(illegal_sequence: exn option) (dest: utf32_string) (index: int ref) (code: int): unit = (
	ignore illegal_sequence; (* without checking surrogate pair in set *)
	Bigarray.Array1.set dest !index (UCS4.of_int code);
	incr index
);;

let utf32_lead (_: utf32_string) (i: int): int = i;;

let utf8_of_utf16 ?(illegal_sequence: exn option) (source: utf16_string): utf8_string = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bytes.unsafe_to_string (Bytes.create (3 * source_length)) in
	let rec make illegal_sequence length source i result j = (
		if !i >= length then String.sub result 0 !j else (
			let code = utf16_get_code ?illegal_sequence source i in
			utf8_set_code ?illegal_sequence result j code;
			make illegal_sequence length source i result j
		)
	) in
	make illegal_sequence source_length source (ref 0) result (ref 0)
);;

let utf8_of_utf32 ?(illegal_sequence: exn option) (source: utf32_string): utf8_string = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bytes.unsafe_to_string (Bytes.create (6 * source_length)) in
	let rec make illegal_sequence length source i result j = (
		if !i >= length then String.sub result 0 !j else (
			let code = utf32_get_code ?illegal_sequence source i in
			utf8_set_code ?illegal_sequence result j code;
			make illegal_sequence length source i result j
		)
	) in
	make illegal_sequence source_length source (ref 0) result (ref 0)
);;

let utf16_of_utf8 ?(illegal_sequence: exn option) (source: utf8_string): utf16_string = (
	let source_length = String.length source in
	let result = Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout source_length in
	let rec make illegal_sequence length source i result j = (
		if !i >= length then Bigarray.Array1.sub result 0 !j else (
			let code = utf8_get_code ?illegal_sequence source i in
			utf16_set_code ?illegal_sequence result j code;
			make illegal_sequence length source i result j
		)
	) in
	make illegal_sequence source_length source (ref 0) result (ref 0)
);;

let utf16_of_utf32 ?(illegal_sequence: exn option) (source: utf32_string): utf16_string = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bigarray.Array1.create Bigarray.int16_unsigned Bigarray.c_layout (2 * source_length) in
	let rec make illegal_sequence length source i result j = (
		if !i >= length then Bigarray.Array1.sub result 0 !j else (
			let code = utf32_get_code ?illegal_sequence source i in
			utf16_set_code ?illegal_sequence result j code;
			make illegal_sequence length source i result j
		)
	) in
	make illegal_sequence source_length source (ref 0) result (ref 0)
);;

let utf32_of_utf8 ?(illegal_sequence: exn option) (source: utf8_string): utf32_string = (
	let source_length = String.length source in
	let result = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout source_length in
	let rec make illegal_sequence length source i result j = (
		if !i >= length then Bigarray.Array1.sub result 0 !j else (
			let code = utf8_get_code ?illegal_sequence source i in
			utf32_set_code ?illegal_sequence result j code;
			make illegal_sequence length source i result j
		)
	) in
	make illegal_sequence source_length source (ref 0) result (ref 0)
);;

let utf32_of_utf16 ?(illegal_sequence: exn option) (source: utf16_string): utf32_string = (
	let source_length = Bigarray.Array1.dim source in
	let result = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout source_length in
	let rec make illegal_sequence length source i result j = (
		if !i >= length then Bigarray.Array1.sub result 0 !j else (
			let code = utf16_get_code ?illegal_sequence source i in
			utf32_set_code ?illegal_sequence result j code;
			make illegal_sequence length source i result j
		)
	) in
	make illegal_sequence source_length source (ref 0) result (ref 0)
);;

module UTF8 = struct
	type elm = utf8_char;;
	include String;;
	external set: t -> int -> char -> unit = "%string_safe_set";;
	external unsafe_set: t -> int -> char -> unit = "%string_unsafe_set";;
	let empty = "";;
	external create: int -> t = "caml_create_string";;
	let append = ( ^ );;
	let fill s ofs len c = Bytes.fill (Bytes.unsafe_of_string s) ofs len c;;
	external unsafe_fill: t -> int -> int -> char -> unit = "caml_fill_string" "noalloc"
	let blit s1 ofs1 s2 ofs2 len = Bytes.blit_string s1 ofs1 (Bytes.unsafe_of_string s2) ofs2 len;;
	external unsafe_blit: t -> int -> t -> int -> int -> unit = "caml_blit_string" "noalloc"
	let sequence = utf8_sequence;;
	let max_sequence = 6;;
	let get_code = utf8_get_code;;
	let set_code = utf8_set_code;;
	let lead = utf8_lead;;
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
	let sequence = utf16_sequence;;
	let max_sequence = 2;;
	let get_code = utf16_get_code;;
	let set_code = utf16_set_code;;
	let lead = utf16_lead;;
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
	let sequence = utf32_sequence;;
	let max_sequence = 1;;
	let get_code = utf32_get_code;;
	let set_code = utf32_set_code;;
	let lead = utf32_lead;;
	let of_utf8 = utf32_of_utf8;;
	let of_utf16 = utf32_of_utf16;;
	let of_array = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout;;
end;;
