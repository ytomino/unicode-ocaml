let fail loc _ = failwith loc;;

let utf8_recovery: Unicode.utf8_string -> int -> int ->
	Unicode.utf8_decode_error -> Uchar.t =
	let rec tails source pos end_pos offset code = (
		if offset <= 0 || pos >= end_pos then Uchar.unsafe_of_int (code lsl offset)
		else
		let tail = int_of_char (String.unsafe_get source pos) in
		tails source (pos + 1) end_pos (offset - 6)
			(code lsl 6 lor (tail land 0b00111111))
	) in
	fun source pos end_pos error ->
	match error with
	| `illegal_sequence ->
		(* Use out of meaning code points, by way of precaution. *)
		let lead = int_of_char (String.unsafe_get source pos) in
		Uchar.unsafe_of_int (lead + 0x7fffff00)
	| `overly_long (`some x) ->
		x
	| `over_17planes x
	| `overly_long (`over_17planes x | `surrogate_fragment x)
	| `surrogate_fragment x ->
		Uchar.unsafe_of_int x
	| `truncated ->
		(* Calculate with supposition that the trailing is 0. *)
		let lead = int_of_char (String.unsafe_get source pos) in
		if lead land 0b10000000 = 0 then (
			Uchar.unsafe_of_int lead
		) else if lead land 0b11100000 = 0b11000000 then (
			tails source (pos + 1) end_pos 6 (lead land 0b00011111)
		) else if lead land 0b11110000 = 0b11100000 then (
			tails source (pos + 1) end_pos 12 (lead land 0b00001111)
		) else if lead land 0b11111000 = 0b11110000 then (
			tails source (pos + 1) end_pos 18 (lead land 0b00000111)
		) else if lead land 0b11111100 = 0b11111000 then (
			tails source (pos + 1) end_pos 24 (lead land 0b00000011)
		) else if lead land 0b11111110 = 0b11111100 then (
			tails source (pos + 1) end_pos 30 (lead land 0b00000001)
		) else Uchar.unsafe_of_int (lead + 0x7fffff00);;

let utf16_recovery (_: Unicode.utf16_string) (_: int) (_: int)
	(error: Unicode.utf16_decode_error) =
(
	match error with
	| `surrogate_fragment x ->
		(* Calculate with supposition that the trailing is 0. *)
		if x land 0xfc00 = 0xd800
		then Uchar.unsafe_of_int (((x land (1 lsl 10 - 1)) lsl 10) + 0x10000)
		else Uchar.unsafe_of_int x
);;

let utf32_recovery (source: Unicode.utf32_string) (pos: int) (_: int)
	(error: Unicode.utf32_decode_error) =
(
	match error with
	| `illegal_sequence ->
		Uchar.unsafe_of_int
			(Unicode.Uint32.to_int (Unicode.UTF32.unsafe_get source pos))
	| `over_17planes x | `surrogate_fragment x ->
		Uchar.unsafe_of_int x
);;

(* utfX_of_utfX *)

let data8 = "あいうえお𐄷";; (* A, I, U, E, O, "AEGEAN WEIGHT BASE UNIT" *)
let data16 = Unicode.utf16_of_utf8 ~fail:(fail __LOC__) data8;;
let data32 = Unicode.utf32_of_utf8 ~fail:(fail __LOC__) data8;;

assert (Unicode.utf16_of_utf8 ~fail:(fail __LOC__) data8 = data16);;
assert (Unicode.utf32_of_utf8 ~fail:(fail __LOC__) data8 = data32);;

assert (Unicode.utf8_of_utf16 ~fail:(fail __LOC__) data16 = data8);;
assert (Unicode.utf8_of_utf32 ~fail:(fail __LOC__) data32 = data8);;
assert (Unicode.utf8_of_utf32 ~fail:(fail __LOC__) data32 = data8);;
assert (Unicode.utf16_of_utf32 ~fail:(fail __LOC__) data32 = data16);;
assert (Unicode.utf32_of_utf16 ~fail:(fail __LOC__) data16 = data32);;

assert (
	Unicode.utf16_of_utf8
		~fail:(fun source old_i i error ->
			assert (old_i = 0 && i = 1);
			match error with
			| `illegal_sequence -> (* at first *)
				utf8_recovery source old_i i `illegal_sequence
			| `unexist -> (* at second *)
				Uchar.of_int 0x10ffff
			| `over_17planes _ | `overly_long _ | `surrogate_fragment _ | `truncated ->
				assert false
		)
		"\xff"
	= Unicode.UTF16.of_array [| 0xdbff; 0xdfff |]
);

(* lead/rear *)

let rec iter_check length lead rear i j source indexes = (
	let source_length = length source in
	if i < source_length then (
		assert (lead source i = indexes.(j));
		let nj = j + 1 in
		let next = if nj < Array.length indexes then indexes.(nj) else source_length in
		let expected_rear = next - 1 in
		assert (rear source i = expected_rear);
		let ni = i + 1 in
		iter_check length lead rear ni (if ni = next then nj else j) source indexes
	)
) in
let iter_check_utf8 =
	let open Unicode.UTF8 in iter_check length lead rear 0 0
in
let iter_check_utf16 =
	let open Unicode.UTF16 in iter_check length lead rear 0 0
in
let iter_check_utf32 =
	let open Unicode.UTF32 in iter_check length lead rear 0 0
in
iter_check_utf8 data8 [| 0; 3; 6; 9; 12; 15 |];
iter_check_utf16 data16 [| 0; 1; 2; 3; 4; 5 |];
iter_check_utf32 data32 [| 0; 1; 2; 3; 4; 5 |];
iter_check_utf8 "#" [| 0 |];
iter_check_utf8 "\x80" [| 0 |];
iter_check_utf8 "\xc0" [| 0 |];
iter_check_utf8 "##" [| 0; 1 |];
iter_check_utf8 "\x80#" [| 0; 1 |];
iter_check_utf8 "\xc0#" [| 0; 1 |];
iter_check_utf8 "#\x80" [| 0; 1 |];
iter_check_utf8 "#\xc0" [| 0; 1 |];
iter_check_utf8 "\x80\x80" [| 0; 1 |];
iter_check_utf8 "\xc0\xc0" [| 0; 1 |];
iter_check_utf8 "\xc0\x80\x80" [| 0; 2 |];
iter_check_utf8 "\xc0\x80\x80\x80" [| 0; 2; 3 |];
iter_check_utf8 "\xc0\x80\x80\xc0" [| 0; 2; 3 |];
iter_check_utf8 "\xc0\x80\xc0\x80" [| 0; 2 |];
iter_check_utf8 "\xc0\x80\xc0\xc0" [| 0; 2; 3 |];
iter_check_utf8 "#\xfc\x80\x80\x80\x80\x80" [| 0; 1 |];
iter_check_utf8 "\xfc\x80\x80\x80\x80\x80#" [| 0; 6 |];
iter_check_utf8 "\xfc\x80\x80\x80\x80\x80\x80" [| 0; 6; 7 |];
let pair_data16 =
	Unicode.utf16_of_utf32 ~fail:(fail __LOC__) (
		Unicode.UTF32.of_array
			(Array.map Unicode.Uint32.of_int32 [| 0x10000l; 0x10001l; 0x10002l |])
	)
in
iter_check_utf16 pair_data16 [| 0; 2; 4 |];
let utf16_of_array = Unicode.UTF16.of_array in
iter_check_utf16 (utf16_of_array [| 0xffff |]) [| 0 |];
iter_check_utf16 (utf16_of_array [| 0xdc00 |]) [| 0 |];
iter_check_utf16 (utf16_of_array [| 0xd800 |]) [| 0 |];
iter_check_utf16 (utf16_of_array [| 0xffff; 0xffff |]) [| 0; 1 |];
iter_check_utf16 (utf16_of_array [| 0xffff; 0xd800 |]) [| 0; 1 |];
iter_check_utf16 (utf16_of_array [| 0xffff; 0xdc00 |]) [| 0; 1 |];
iter_check_utf16 (utf16_of_array [| 0xd800; 0xffff |]) [| 0; 1 |];
iter_check_utf16 (utf16_of_array [| 0xdc00; 0xffff |]) [| 0; 1 |];
iter_check_utf16 (utf16_of_array [| 0xd800; 0xd800 |]) [| 0; 1 |];
iter_check_utf16 (utf16_of_array [| 0xdc00; 0xdc00 |]) [| 0; 1 |];
iter_check_utf16 (utf16_of_array [| 0xdc00; 0xd800; 0xdc00 |]) [| 0; 1 |];
iter_check_utf16 (utf16_of_array [| 0xd800; 0xdc00; 0xdc00 |]) [| 0; 2 |];;

(* UTF-8 corners *)

let utf8_over_17planes reported source old_i i error = (
	match error with
	| `over_17planes _ ->
		reported := true;
		utf8_recovery source old_i i error
	| `illegal_sequence | `overly_long _ | `surrogate_fragment _ | `truncated ->
		assert false
);;

assert (
	let s = "\x7f" in
	let i = ref 0 in
	Unicode.utf8_get_code ~fail:(fail __LOC__) s i = Uchar.of_int 0x007f && !i = 1
);;

assert (
	let s = "\xc2\x80" in
	let i = ref 0 in
	Unicode.utf8_get_code ~fail:(fail __LOC__) s i = Uchar.of_int 0x0080 && !i = 2
);;

assert (
	let s = "\xdf\xbf" in
	let i = ref 0 in
	Unicode.utf8_get_code ~fail:(fail __LOC__) s i = Uchar.of_int 0x07ff && !i = 2
);;

assert (
	let s = "\xe0\xa0\x80" in
	let i = ref 0 in
	Unicode.utf8_get_code ~fail:(fail __LOC__) s i = Uchar.of_int 0x0800 && !i = 3
);;

assert (
	let s = "\xef\xbf\xbf" in
	let i = ref 0 in
	Unicode.utf8_get_code ~fail:(fail __LOC__) s i = Uchar.of_int 0xffff && !i = 3
);;

assert (
	let s = "\xf0\x90\x80\x80" in
	let i = ref 0 in
	Unicode.utf8_get_code ~fail:(fail __LOC__) s i = Uchar.of_int 0x10000 && !i = 4
);;

assert (
	let s = "\xf4\x8f\xbf\xbf" in
	let i = ref 0 in
	Unicode.utf8_get_code ~fail:(fail __LOC__) s i = Uchar.of_int 0x10ffff
	&& !i = 4
);;

assert (
	let s = "\xf4\x90\x80\x80" in
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_over_17planes reported) s i
		= Uchar.unsafe_of_int 0x110000
	&& !i = 4
	&& !reported
);;

assert (
	let s = "\xf7\xbf\xbf\xbf" in
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_over_17planes reported) s i
		= Uchar.unsafe_of_int 0x1fffff
	&& !i = 4
	&& !reported
);;

assert (
	let s = "\xf8\x88\x80\x80\x80" in
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_over_17planes reported) s i
		= Uchar.unsafe_of_int 0x200000
	&& !i = 5
	&& !reported
);;

assert (
	let s = "\xfb\xbf\xbf\xbf\xbf" in
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_over_17planes reported) s i
		= Uchar.unsafe_of_int 0x3ffffff
	&& !i = 5
	&& !reported
);;

assert (
	let s = "\xfc\x84\x80\x80\x80\x80" in
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_over_17planes reported) s i =
		Uchar.unsafe_of_int 0x4000000
	&& !i = 6
	&& !reported
);;

assert (
	let s = "\xfd\xbf\xbf\xbf\xbf\xbf" in
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_over_17planes reported) s i
		= Uchar.unsafe_of_int 0x7fffffff
	&& !i = 6
	&& !reported
);;

(* UTF-8 illegal sequence *)

let utf8_illegal_sequence reported source old_i i error = (
	match error with
	| `illegal_sequence ->
		reported := true;
		utf8_recovery source old_i i `illegal_sequence
	| `over_17planes _ | `overly_long _ | `surrogate_fragment _ | `truncated ->
		assert false
);;

let utf8_truncated reported source old_i i error = (
	match error with
	| `truncated ->
		reported := true;
		utf8_recovery source old_i i `truncated
	| `illegal_sequence | `over_17planes _ | `overly_long _
	| `surrogate_fragment _ ->
		assert false
);;

let iseq8_1 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001 |];; (* few *)
let iseq8_2 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000 |];; (* few *)
let iseq8_3 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000;
	char_of_int 0b10000000 |];; (* few *)
let iseq8_4 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000;
	char_of_int 0b10000000;
	char_of_int 0b10000000 |];; (* just *)
let iseq8_5 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000;
	char_of_int 0b10000000;
	char_of_int 0b10000000;
	char_of_int 0b10000000 |];; (* remainder *)
let iseq8_6 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000; (* few *)
	char_of_int 0b11000000 |];; (* next leading *)
let iseq8_7 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000;
	char_of_int 0b10000000; (* few *)
	char_of_int 0b11000000 |];; (* next leading *)
let iseq8_8 = Unicode.UTF8.of_array [|
	char_of_int 0b10101010 |];; (* start from trailing byte *)

assert (
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_truncated reported) iseq8_1 i
		= Uchar.of_int (1 lsl 18)
	&& !i = 1
	&& !reported
);;
assert (
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_truncated reported) iseq8_2 i
		= Uchar.of_int (1 lsl 18)
	&& !i = 2
	&& !reported
);;
assert (
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_truncated reported) iseq8_3 i
		= Uchar.of_int (1 lsl 18)
	&& !i = 3
	&& !reported
);;
assert (
	let i = ref 0 in
	Unicode.utf8_get_code ~fail:(fail __LOC__) iseq8_4 i = Uchar.of_int (1 lsl 18)
	&& !i = 4
);;
assert (
	let i = ref 0 in
	Unicode.utf8_get_code ~fail:(fail __LOC__) iseq8_5 i = Uchar.of_int (1 lsl 18)
	&& !i = 4
);;
assert (
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_truncated reported) iseq8_6 i
		= Uchar.of_int (1 lsl 18)
	&& !i = 2
	&& !reported
);;
assert (
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_truncated reported) iseq8_7 i
		= Uchar.of_int (1 lsl 18)
	&& !i = 3
	&& !reported
);;
assert (
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf8_get_code ~fail:(utf8_illegal_sequence reported) iseq8_8 i
		= Uchar.unsafe_of_int 0x7fffffaa
	&& !i = 1
	&& !reported
);;

(* UTF-16 illegal sequence *)

let utf16_surrogate_fragment reported source old_i i error = (
	match error with
	| `surrogate_fragment _ ->
		reported := true;
		utf16_recovery source old_i i error
);;

let iseq16_1 = Unicode.UTF16.of_array
	[| 0xd800 |];; (* few *)
let iseq16_2 = Unicode.UTF16.of_array
	[| 0xd800; 0xdc00 |];; (* just *)
let iseq16_3 = Unicode.UTF16.of_array
	[| 0xd800; 0xdc00; 0xdc00 |];; (* remainder *)
let iseq16_4 = Unicode.UTF16.of_array
	[| 0xd800; 0xd800 |];; (* few and next leading *)
let iseq16_5 = Unicode.UTF16.of_array
	[| 0xdeaa |];; (* start from trailing byte *)

assert (
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf16_get_code ~fail:(utf16_surrogate_fragment reported) iseq16_1 i
		= Uchar.of_int 0x10000
	&& !i = 1
	&& !reported
);;
assert (
	let i = ref 0 in
	Unicode.utf16_get_code ~fail:(fail __LOC__) iseq16_2 i = Uchar.of_int 0x10000
	&& !i = 2
);;
assert (
	let i = ref 0 in
	Unicode.utf16_get_code ~fail:(fail __LOC__) iseq16_3 i = Uchar.of_int 0x10000
	&& !i = 2
);;
assert (
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf16_get_code ~fail:(utf16_surrogate_fragment reported) iseq16_4 i
		= Uchar.of_int 0x10000
	&& !i = 1
	&& !reported
);;
assert (
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf16_get_code ~fail:(utf16_surrogate_fragment reported) iseq16_5 i
		= Uchar.unsafe_of_int 0xdeaa
	&& !i = 1
	&& !reported
);;

(* UTF-32 corners *)

let utf32_over_17planes reported source old_i i error = (
	match error with
	| `over_17planes _ ->
		reported := true;
		utf32_recovery source old_i i error
	| `illegal_sequence | `surrogate_fragment _ ->
		assert false
);;

assert (
	let s = Unicode.UTF32.of_array [| Unicode.Uint32.of_int32 0x0010ffffl |] in
	let i = ref 0 in
	Unicode.utf32_get_code ~fail:(fail __LOC__) s i = Uchar.unsafe_of_int 0x10ffff
	&& !i = 1
);;

assert (
	let s = Unicode.UTF32.of_array [| Unicode.Uint32.of_int32 0x00110000l |] in
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf32_get_code ~fail:(utf32_over_17planes reported) s i
		= Uchar.unsafe_of_int 0x110000
	&& !i = 1
	&& !reported
);;

(* UTF-32 illegal sequence *)

let utf32_illegal_sequence reported source old_i i error = (
	match error with
	| `illegal_sequence ->
		reported := true;
		utf32_recovery source old_i i `illegal_sequence
	| `over_17planes _ | `surrogate_fragment _ ->
		assert false
);;

let iseq32_1 = Unicode.UTF32.of_array
	(Array.map Unicode.Uint32.of_int32 [| 0xffffffffl |]);;
let iseq32_2 = Unicode.UTF32.of_array
	(Array.map Unicode.Uint32.of_int32 [| 0xfffffffel |]);;

assert (
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf32_get_code ~fail:(utf32_illegal_sequence reported) iseq32_1 i
		= Uchar.unsafe_of_int (if Sys.word_size <= 32 then ~-1 else 1 lsl 32 - 1)
	&& !i = 1
	&& !reported
);;
assert (
	let i = ref 0 in
	let reported = ref false in
	Unicode.utf32_get_code ~fail:(utf32_illegal_sequence reported) iseq32_2 i
		= Uchar.unsafe_of_int (if Sys.word_size <= 32 then ~-2 else 1 lsl 32 - 2)
	&& !i = 1
	&& !reported
);;

(* encode U+10FFFF *)

let last_uchar = Uchar.of_int 0x10ffff;;

assert (
	let r =
		Unicode.utf8_encode (fun () b item -> Buffer.add_char b item; b)
			~fail:(fail __LOC__) () (Buffer.create 4) last_uchar
	in
	Buffer.contents r = "\xf4\x8f\xbf\xbf"
);;

assert (
	let r =
		Unicode.utf16_encode (fun () b item -> item :: b) ~fail:(fail __LOC__) () []
			last_uchar
	in
	List.rev r = [0xdbff; 0xdfff]
);;

assert (
	let r =
		Unicode.utf32_encode (fun () b item -> item :: b) ~fail:(fail __LOC__) () []
			last_uchar
	in
	r = [Unicode.Uint32.of_int32 0x10ffffl]
);;

(* encode U+110000 *)

let lastp1_uchar = Uchar.unsafe_of_int 0x110000;;

assert (
	let r =
		Unicode.utf8_encode (fun () b item -> Buffer.add_char b item; b)
			~fail:(fail __LOC__) () (Buffer.create 4) lastp1_uchar
	in
	Buffer.contents r = "\xf4\x90\x80\x80"
);;

assert (
	Unicode.utf16_encode (fun () _ _ -> assert false)
		~fail:(fun () _ error -> assert (error = `unexist); ~-1) () 0 lastp1_uchar
	= ~-1
);;

assert (
	let r =
		Unicode.utf32_encode (fun () b item -> item :: b) ~fail:(fail __LOC__) () []
			lastp1_uchar
	in
	r = [Unicode.Uint32.of_int32 0x110000l]
);;

(* encode U+7FFFFFFF *)

let max_uchar = Uchar.unsafe_of_int 0x7fffffff;;

assert (
	let r =
		Unicode.utf8_encode (fun () b item -> Buffer.add_char b item; b)
			~fail:(fail __LOC__) () (Buffer.create 6) max_uchar
	in
	Buffer.contents r = "\xfd\xbf\xbf\xbf\xbf\xbf"
);;

assert (
	Unicode.utf16_encode (fun () _ _ -> assert false)
		~fail:(fun () _ error -> assert (error = `unexist); ~-1) () 0 max_uchar
	= ~-1
);;

assert (
	let r =
		Unicode.utf32_encode (fun () b item -> item :: b) ~fail:(fail __LOC__) () []
			max_uchar
	in
	r = [Unicode.Uint32.of_int32 0x7fffffffl]
);;

if Sys.word_size > 32 then (
	(* encode over 31bits, these values can only exist in UTF-32 *)
	let min_illegal = Uchar.unsafe_of_int (1 lsl 31) in
	assert (
		Unicode.utf8_encode (fun () _ _ -> assert false) ~fail:(fun () _ _ -> ~-1) () 0
			min_illegal
		= ~-1
	);
	assert (
		Unicode.utf16_encode (fun () _ _ -> assert false) ~fail:(fun () _ _ -> ~-1) ()
			0 min_illegal
		= ~-1
	);
	assert (
		let r =
			Unicode.utf32_encode (fun () b item -> item :: b) ~fail:(fail __LOC__) () []
				min_illegal
		in
		r = [Unicode.Uint32.of_int32 0x80000000l]
	);
	let max_illegal = Uchar.unsafe_of_int (1 lsl 32 - 1) in
	assert (
		Unicode.utf8_encode (fun () _ _ -> assert false) ~fail:(fun () _ _ -> ~-1) () 0
			max_illegal
		= ~-1
	);
	assert (
		Unicode.utf16_encode (fun () _ _ -> assert false) ~fail:(fun () _ _ -> ~-1) ()
			0 max_illegal
		= ~-1
	);
	assert (
		let r =
			Unicode.utf32_encode (fun () b item -> item :: b) ~fail:(fail __LOC__) () []
				max_illegal
		in
		r = [Unicode.Uint32.of_int32 0xffffffffl]
	);
	(* encode over 32bits, the results are undefined *)
	let over32 = Uchar.unsafe_of_int (1 lsl 32) in
	ignore (
		Unicode.utf8_encode (fun () b item -> Buffer.add_char b item; b)
			~fail:(fail __LOC__) () (Buffer.create 6) over32
	);
	ignore (
		Unicode.utf16_encode (fun () b item -> item :: b) ~fail:(fail __LOC__) () []
			over32
	);
	ignore (
		Unicode.utf32_encode (fun () b item -> item :: b) ~fail:(fail __LOC__) () []
			over32
	);
	let negative = Uchar.unsafe_of_int ~-1 in
	assert (
		Unicode.utf8_encode (fun () _ _ -> assert false) ~fail:(fun () _ _ -> ~-1) () 0
			negative
		= ~-1
	);
	assert (
		Unicode.utf16_encode (fun () _ _ -> assert false)
			~fail:(fun () _ _ -> ~-1) () 0 negative
		= ~-1
	);
	ignore (
		Unicode.utf32_encode (fun () b item -> item :: b) ~fail:(fail __LOC__) () []
			negative
	)
);;

(* report *)

prerr_endline "ok";;
