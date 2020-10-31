let exn = Failure "test_conv.ml";;

(* utfX_of_utfX *)

let data8 = "あいうえお𐄷";; (* A, I, U, E, O, "AEGEAN WEIGHT BASE UNIT" *)
let data16 = Unicode.utf16_of_utf8 data8;;
let data32 = Unicode.utf32_of_utf8 data8;;

assert (Unicode.utf16_of_utf8 ~illegal_sequence:exn data8 = data16);;
assert (Unicode.utf32_of_utf8 ~illegal_sequence:exn data8 = data32);;

assert (Unicode.utf8_of_utf16 data16 = data8);;
assert (Unicode.utf8_of_utf16 ~illegal_sequence:exn data16 = data8);;
assert (Unicode.utf8_of_utf32 data32 = data8);;
assert (Unicode.utf8_of_utf32 ~illegal_sequence:exn data32 = data8);;
assert (Unicode.utf16_of_utf32 data32 = data16);;
assert (Unicode.utf16_of_utf32 ~illegal_sequence:exn data32 = data16);;
assert (Unicode.utf32_of_utf16 data16 = data32);;
assert (Unicode.utf32_of_utf16 ~illegal_sequence:exn data16 = data32);;

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
let pair_data16 = Unicode.utf16_of_utf32
	(Unicode.UTF32.of_array
		(Array.map Unicode.Uint32.of_int32 [| 0x10000l; 0x10001l; 0x10002l |]))
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

assert (
	let s = "\x7f" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.of_int 0x007f && !i = 1
);;

assert (
	let s = "\xc2\x80" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.of_int 0x0080 && !i = 2
);;

assert (
	let s = "\xdf\xbf" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.of_int 0x07ff && !i = 2
);;

assert (
	let s = "\xe0\xa0\x80" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.of_int 0x0800 && !i = 3
);;

assert (
	let s = "\xef\xbf\xbf" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.of_int 0xffff && !i = 3
);;

assert (
	let s = "\xf0\x90\x80\x80" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.of_int 0x10000
		&& !i = 4
);;

assert (
	let s = "\xf4\x8f\xbf\xbf" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.of_int 0x10ffff
		&& !i = 4
);;

assert (
	let s = "\xf4\x90\x80\x80" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.unsafe_of_int 0x110000
		&& !i = 4
);;

assert (
	let s = "\xf7\xbf\xbf\xbf" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.unsafe_of_int 0x1fffff
		&& !i = 4
);;

assert (
	let s = "\xf8\x88\x80\x80\x80" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.unsafe_of_int 0x200000
		&& !i = 5
);;

assert (
	let s = "\xfb\xbf\xbf\xbf\xbf" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.unsafe_of_int 0x3ffffff
		&& !i = 5
);;

assert (
	let s = "\xfc\x84\x80\x80\x80\x80" in
	let i = ref 0 in
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.unsafe_of_int 0x4000000
		&& !i = 6
);;

assert (
	let s = "\xfd\xbf\xbf\xbf\xbf\xbf" in
	let i = ref 0 in
	let r = Unicode.utf8_get_code ~illegal_sequence:exn s i in
	r = Uchar.unsafe_of_int 0x7fffffff && !i = 6
);;

(* UTF-8 illegal sequence *)

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
	Unicode.utf8_get_code iseq8_1 i = Uchar.of_int (1 lsl 18) && !i = 1
);;
assert (
	let i = ref 0 in
	Unicode.utf8_get_code iseq8_2 i = Uchar.of_int (1 lsl 18) && !i = 2
);;
assert (
	let i = ref 0 in
	Unicode.utf8_get_code iseq8_3 i = Uchar.of_int (1 lsl 18) && !i = 3
);;
assert (
	let i = ref 0 in
	Unicode.utf8_get_code iseq8_4 i = Uchar.of_int (1 lsl 18) && !i = 4
);;
assert (
	let i = ref 0 in
	Unicode.utf8_get_code iseq8_5 i = Uchar.of_int (1 lsl 18) && !i = 4
);;
assert (
	let i = ref 0 in
	Unicode.utf8_get_code iseq8_6 i = Uchar.of_int (1 lsl 18) && !i = 2
);;
assert (
	let i = ref 0 in
	Unicode.utf8_get_code iseq8_7 i = Uchar.of_int (1 lsl 18) && !i = 3
);;
assert (
	let i = ref 0 in
	Unicode.utf8_get_code iseq8_8 i = Uchar.unsafe_of_int 0x7fffffaa && !i = 1
);;

(* UTF-16 illegal sequence *)

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
	Unicode.utf16_get_code iseq16_1 i = Uchar.of_int 0x10000 && !i = 1
);;
assert (
	let i = ref 0 in
	Unicode.utf16_get_code iseq16_2 i = Uchar.of_int 0x10000 && !i = 2
);;
assert (
	let i = ref 0 in
	Unicode.utf16_get_code iseq16_3 i = Uchar.of_int 0x10000 && !i = 2
);;
assert (
	let i = ref 0 in
	Unicode.utf16_get_code iseq16_4 i = Uchar.of_int 0x10000 && !i = 1
);;
assert (
	let i = ref 0 in
	Unicode.utf16_get_code iseq16_5 i = Uchar.unsafe_of_int 0xdeaa && !i = 1
);;

(* UTF-32 illegal sequence *)

let iseq32_1 = Unicode.UTF32.of_array
	(Array.map Unicode.Uint32.of_int32 [| 0xffffffffl |]);;
let iseq32_2 = Unicode.UTF32.of_array
	(Array.map Unicode.Uint32.of_int32 [| 0xfffffffel |]);;

assert (
	let i = ref 0 in
	let r = Unicode.utf32_get_code iseq32_1 i in
	r = Uchar.unsafe_of_int (if Sys.word_size <= 32 then ~-1 else 1 lsl 32 - 1)
		&& !i = 1
);;
assert (
	let i = ref 0 in
	let r = Unicode.utf32_get_code iseq32_2 i in
	r = Uchar.unsafe_of_int (if Sys.word_size <= 32 then ~-2 else 1 lsl 32 - 2)
		&& !i = 1
);;

(* encode U+7FFFFFFF *)

let max_uchar = Uchar.unsafe_of_int 0x7fffffff;;

assert (
	let r = Unicode.utf8_encode (fun () b item -> Buffer.add_char b item; b)
		() (Buffer.create 6) max_uchar
	in
	Buffer.contents r = "\xfd\xbf\xbf\xbf\xbf\xbf"
);;

assert (
	let r = Unicode.utf16_encode (fun () b item -> item :: b) () [] max_uchar in
	List.rev r = [0xdbff; 0xdfff]
);;

assert (
	let r = Unicode.utf32_encode (fun () b item -> item :: b) () [] max_uchar in
	r = [Unicode.Uint32.of_int32 0x7fffffffl]
);;

if Sys.word_size > 32 then (
	(* encode over 31bits, these values can only exist in UTF-32 *)
	let min_illegal = Uchar.unsafe_of_int (1 lsl 31) in
	assert (
		let r = Unicode.utf8_encode (fun () b item -> Buffer.add_char b item; b)
			() (Buffer.create 6) min_illegal
		in
		Buffer.contents r = "\xfd\xbf\xbf\xbf\xbf\xbf"
	);
	assert (
		let r = Unicode.utf16_encode (fun () b item -> item :: b) () [] min_illegal in
		List.rev r = [0xdbff; 0xdfff]
	);
	assert (
		let r = Unicode.utf32_encode (fun () b item -> item :: b) () [] min_illegal in
		r = [Unicode.Uint32.of_int32 0x80000000l]
	);
	let max_illegal = Uchar.unsafe_of_int (1 lsl 32 - 1) in
	assert (
		let r = Unicode.utf8_encode (fun () b item -> Buffer.add_char b item; b)
			() (Buffer.create 6) max_illegal
		in
		Buffer.contents r = "\xfd\xbf\xbf\xbf\xbf\xbf"
	);
	assert (
		let r = Unicode.utf16_encode (fun () b item -> item :: b) () [] max_illegal in
		List.rev r = [0xdbff; 0xdfff]
	);
	assert (
		let r = Unicode.utf32_encode (fun () b item -> item :: b) () [] max_illegal in
		r = [Unicode.Uint32.of_int32 0xffffffffl]
	);
	(* encode over 32bits, the results are undefined *)
	let over32 = Uchar.unsafe_of_int (1 lsl 32) in
	ignore (
		Unicode.utf8_encode (fun () b item -> Buffer.add_char b item; b) ()
			(Buffer.create 6) over32
	);
	ignore (Unicode.utf16_encode (fun () b item -> item :: b) () [] over32);
	ignore (Unicode.utf32_encode (fun () b item -> item :: b) () [] over32);
	let negative = Uchar.unsafe_of_int ~-1 in
	ignore (
		Unicode.utf8_encode (fun () b item -> Buffer.add_char b item; b) ()
			(Buffer.create 6) negative
	);
	ignore (Unicode.utf16_encode (fun () b item -> item :: b) () [] negative);
	ignore (Unicode.utf32_encode (fun () b item -> item :: b) () [] negative)
);;

(* report *)

prerr_endline "ok";;
