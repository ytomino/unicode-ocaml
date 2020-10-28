open Unicode;;

(* UTF8/UTF8_Bytes.of_array *)

assert (UTF8.of_array [| |] = "");;
assert (UTF8.of_array [| 'A' |] = "A");;
assert (UTF8.of_array [| 'A'; 'B' |] = "AB");;

(* UTF32.of_array *)

assert (UTF32.of_array [| |] = UTF32.empty);;
assert (UTF32.of_array [| Uint32.of_int 0x41 |] = UTF32.of_utf8 "A");;
assert (
	let r = UTF32.of_array [| Uint32.of_int 0x41 ; Uint32.of_int 0x42 |] in
	r = UTF32.of_utf8 "AB"
);;

(* UTF16/32.copy *)

let a = utf16_of_utf8 "ABC" in
let b = UTF16.copy a in
assert (a = b && a != b);;

(* UTF8_Bytes.append *)

let a = Bytes.of_string "ABC" in
let b = Bytes.of_string "DEF" in
assert (UTF8_Bytes.append a b = Bytes.of_string "ABCDEF");;
let a = Bytes.of_string "AB" in
let b = Bytes.of_string "CDEF" in
assert (UTF8_Bytes.append a b = Bytes.of_string "ABCDEF");;

(* UTF16/32.append *)

let a = utf16_of_utf8 "ABC" in
let b = utf16_of_utf8 "DEF" in
assert (UTF16.append a b = utf16_of_utf8 "ABCDEF");;
let a = utf32_of_utf8 "AB" in
let b = utf32_of_utf8 "CDEF" in
assert (UTF32.append a b = utf32_of_utf8 "ABCDEF");;

(* UTF16/32.fill *)

let a = utf16_of_utf8 "DEADBEAF" in
UTF16.fill a 0 8 (Char.code 'z');
assert (a = utf16_of_utf8 "zzzzzzzz");
UTF16.fill a 1 6 (Char.code 'X');
assert (a = utf16_of_utf8 "zXXXXXXz");;

(* UTF16/32.blit *)

let a = utf16_of_utf8 "ABC" in
let b = utf16_of_utf8 "DEF" in
UTF16.blit a 0 b 0 3;
assert (b = utf16_of_utf8 "ABC");
UTF16.blit a 0 b 1 2;
assert (b = utf16_of_utf8 "AAB");
UTF16.blit a 1 b 0 2;
assert (b = utf16_of_utf8 "BCB");;

(* report *)

prerr_endline "ok";;
