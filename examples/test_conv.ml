(* utfX_of_utfX *)

let data8 = "あいうえお𐄷";; (* A, I, U, E, O, "AEGEAN WEIGHT BASE UNIT" *)
let data16 = Unicode.utf16_of_utf8 data8;;
let data32 = Unicode.utf32_of_utf8 data8;;

assert (Unicode.utf8_of_utf16 data16 = data8);;
assert (Unicode.utf8_of_utf32 data32 = data8);;
assert (Unicode.utf16_of_utf32 data32 = data16);;
assert (Unicode.utf32_of_utf16 data16 = data32);;

(* lead *)

assert (Unicode.utf8_lead data8 6 = 6);;
assert (Unicode.utf8_lead data8 5 = 3);;
assert (Unicode.utf8_lead data8 4 = 3);;
assert (Unicode.utf8_lead data8 3 = 3);;
assert (Unicode.utf8_lead data8 2 = 0);;

let pair_data16 = Unicode.utf16_of_utf32 (Unicode.UTF32.of_array [| 0x10000l; 0x10001l; 0x10002l |]);;
assert (Unicode.utf16_lead pair_data16 5 = 4);;
assert (Unicode.utf16_lead pair_data16 4 = 4);;
assert (Unicode.utf16_lead pair_data16 3 = 2);;
assert (Unicode.utf16_lead pair_data16 2 = 2);;
assert (Unicode.utf16_lead pair_data16 1 = 0);;
assert (Unicode.utf16_lead pair_data16 0 = 0);;

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

assert (let i = ref 0 in Unicode.utf8_get_code iseq8_1 i = Uchar.of_int (1 lsl 18) && !i = 1);;
assert (let i = ref 0 in Unicode.utf8_get_code iseq8_2 i = Uchar.of_int (1 lsl 18) && !i = 2);;
assert (let i = ref 0 in Unicode.utf8_get_code iseq8_3 i = Uchar.of_int (1 lsl 18) && !i = 3);;
assert (let i = ref 0 in Unicode.utf8_get_code iseq8_4 i = Uchar.of_int (1 lsl 18) && !i = 4);;
assert (let i = ref 0 in Unicode.utf8_get_code iseq8_5 i = Uchar.of_int (1 lsl 18) && !i = 4);;
assert (let i = ref 0 in Unicode.utf8_get_code iseq8_6 i = Uchar.of_int (1 lsl 18) && !i = 2);;
assert (let i = ref 0 in Unicode.utf8_get_code iseq8_7 i = Uchar.of_int (1 lsl 18) && !i = 3);;
assert (let i = ref 0 in Unicode.utf8_get_code iseq8_8 i = Uchar.unsafe_of_int 0x7fffffaa && !i = 1);;

(* UTF-16 illegal sequence *)

let iseq16_1 = Unicode.UTF16.of_array [| 0xd800 |];; (* few *)
let iseq16_2 = Unicode.UTF16.of_array [| 0xd800; 0xdc00 |];; (* just *)
let iseq16_3 = Unicode.UTF16.of_array [| 0xd800; 0xdc00; 0xdc00 |];; (* remainder *)
let iseq16_4 = Unicode.UTF16.of_array [| 0xd800; 0xd800 |];; (* few and next leading *)
let iseq16_5 = Unicode.UTF16.of_array [| 0xdeaa |];; (* start from trailing byte *)

assert (let i = ref 0 in Unicode.utf16_get_code iseq16_1 i = Uchar.of_int 0x10000 && !i = 1);;
assert (let i = ref 0 in Unicode.utf16_get_code iseq16_2 i = Uchar.of_int 0x10000 && !i = 2);;
assert (let i = ref 0 in Unicode.utf16_get_code iseq16_3 i = Uchar.of_int 0x10000 && !i = 2);;
assert (let i = ref 0 in Unicode.utf16_get_code iseq16_4 i = Uchar.of_int 0x10000 && !i = 1);;
assert (let i = ref 0 in Unicode.utf16_get_code iseq16_5 i = Uchar.unsafe_of_int 0xdeaa && !i = 1);;

(* UTF-32 illegal sequence *)

let iseq32_1 = Unicode.UTF32.of_array [| 0xffffffffl |];;

assert (let i = ref 0 in Unicode.utf32_get_code iseq32_1 i = Uchar.unsafe_of_int 0x7fffffff);;

(* report *)

print_string "ok";;
print_newline ();;
