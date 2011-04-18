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

let pair_data16 = Unicode.utf16_of_utf32 (Unicode.UTF32.of_array [| 0x10000l; 0x10000l; 0x10000l |]);;
assert (Unicode.utf16_lead pair_data16 4 = 4);;
assert (Unicode.utf16_lead pair_data16 3 = 2);;
assert (Unicode.utf16_lead pair_data16 2 = 2);;
assert (Unicode.utf16_lead pair_data16 1 = 0);;

(* UTF-8 invalid sequence *)

let invalid_data8_1 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001 |];; (* few *)
let invalid_data8_2 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000 |];; (* few *)
let invalid_data8_3 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000;
	char_of_int 0b10000000 |];; (* few *)
let invalid_data8_4 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000;
	char_of_int 0b10000000;
	char_of_int 0b10000000 |];; (* just *)
let invalid_data8_5 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000;
	char_of_int 0b10000000;
	char_of_int 0b10000000;
	char_of_int 0b10000000 |];; (* remainder *)
let invalid_data8_6 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000; (* few *)
	char_of_int 0b11000000 |];; (* next leading *)
let invalid_data8_7 = Unicode.UTF8.of_array [|
	char_of_int 0b11110001;
	char_of_int 0b10000000;
	char_of_int 0b10000000; (* few *)
	char_of_int 0b11000000 |];; (* next leading *)

assert (let i = ref 0 in Unicode.utf8_get_code invalid_data8_1 i = 1 lsl 18 && !i = 1);;
assert (let i = ref 0 in Unicode.utf8_get_code invalid_data8_2 i = 1 lsl 18 && !i = 2);;
assert (let i = ref 0 in Unicode.utf8_get_code invalid_data8_3 i = 1 lsl 18 && !i = 3);;
assert (let i = ref 0 in Unicode.utf8_get_code invalid_data8_4 i = 1 lsl 18 && !i = 4);;
assert (let i = ref 0 in Unicode.utf8_get_code invalid_data8_5 i = 1 lsl 18 && !i = 4);;
assert (let i = ref 0 in Unicode.utf8_get_code invalid_data8_6 i = 1 lsl 18 && !i = 2);;
assert (let i = ref 0 in Unicode.utf8_get_code invalid_data8_7 i = 1 lsl 18 && !i = 3);;
