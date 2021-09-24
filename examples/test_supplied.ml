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

(* UTF32.compare *)

let of_array a = (
	Unicode.UTF32.of_array (Array.map Unicode.Uint32.of_int32 a)
) in
let x_0000 = of_array [| 0x0000l |] in
let x_0001 = of_array [| 0x0001l |] in
let x_fffffffe = of_array [| 0xfffffffel |] in
let x_ffffffff = of_array [| 0xffffffffl |] in
let xs = [
	UTF32.empty;
	x_0000;
	of_array [| 0x0000l; 0x0000l |];
	of_array [| 0x0000l; 0x0001l |];
	x_0001;
	of_array [| 0x7fffffffl |];
	of_array [| 0x7fffffffl; 0x7fffffffl |];
	of_array [| 0x7fffffffl; 0x80000000l |];
	of_array [| 0x80000000l |];
	x_fffffffe;
	of_array [| 0xfffffffel; 0xfffffffel |];
	of_array [| 0xfffffffel; 0xffffffffl |];
	x_ffffffff]
in
assert (UTF32.compare UTF32.empty UTF32.empty = 0);
assert (UTF32.compare UTF32.empty x_0000 < 0);
assert (UTF32.compare x_0000 UTF32.empty > 0);
assert (UTF32.compare x_0000 x_0000 = 0);
assert (UTF32.compare x_0000 x_0001 < 0);
assert (UTF32.compare x_0001 x_0000 > 0);
assert (UTF32.compare x_fffffffe x_ffffffff < 0);
assert (UTF32.compare x_ffffffff x_fffffffe > 0);
assert (UTF32.compare x_ffffffff x_ffffffff = 0);
let ys = List.sort UTF32.compare xs in
List.iter2 (fun a b -> assert (a == b)) xs ys;;

(* UTF16/32.copy *)

let a = utf16_of_utf8 "ABC" in
let b = UTF16.copy a in
assert (a = b && a != b);;

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
