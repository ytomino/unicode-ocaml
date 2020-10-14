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

(* lead *)

assert (Unicode.utf8_lead data8 6 = 6);;
assert (Unicode.utf8_lead data8 5 = 3);;
assert (Unicode.utf8_lead data8 4 = 3);;
assert (Unicode.utf8_lead data8 3 = 3);;
assert (Unicode.utf8_lead data8 2 = 0);;

let pair_data16 = Unicode.utf16_of_utf32
	(Unicode.UTF32.of_array [| 0x10000l; 0x10001l; 0x10002l |]);;
assert (Unicode.utf16_lead pair_data16 5 = 4);;
assert (Unicode.utf16_lead pair_data16 4 = 4);;
assert (Unicode.utf16_lead pair_data16 3 = 2);;
assert (Unicode.utf16_lead pair_data16 2 = 2);;
assert (Unicode.utf16_lead pair_data16 1 = 0);;
assert (Unicode.utf16_lead pair_data16 0 = 0);;

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
	Unicode.utf8_get_code ~illegal_sequence:exn s i = Uchar.unsafe_of_int 0x7fffffff
		&& !i = 6
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

let iseq32_1 = Unicode.UTF32.of_array [| 0xffffffffl |];;

assert (
	let i = ref 0 in
	let r = Unicode.utf32_get_code iseq32_1 i in
	r = Uchar.unsafe_of_int (if Sys.word_size <= 32 then ~-1 else 1 lsl 32 - 1)
		&& !i = 1
);;

(* negative Uchar.t *)

let negative = Uchar.unsafe_of_int ~-1;;

assert (
	let r = Unicode.utf8_encode (fun () b item -> Buffer.add_char b item; b)
		() (Buffer.create 6) negative
	in
	Buffer.contents r = "\xfd\xbf\xbf\xbf\xbf\xbf"
);;

if Sys.word_size > 32 then (
	ignore (
		Unicode.utf8_encode (fun () b item -> Buffer.add_char b item; b) ()
			(Buffer.create 6) (Uchar.unsafe_of_int (1 lsl 32))
	) (* the result is undefined *)
);;

assert (
	let r = Unicode.utf16_encode (fun () b item -> item :: b) () [] negative in
	List.rev r = [0xdbff; 0xdfff]
);;

assert (
	let r = Unicode.utf32_encode (fun () b item -> item :: b) () [] negative in
	r = [if Sys.word_size <= 32 then 0x7fffffffl else 0xffffffffl]
);;

(* report *)

prerr_endline "ok";;
