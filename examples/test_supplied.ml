open Unicode;;

(* UTF8.of_array *)

assert (UTF8.of_array [| |] = "");;
assert (UTF8.of_array [| 'A' |] = "A");;
assert (UTF8.of_array [| 'A'; 'B' |] = "AB");;

(* UTF16/32.copy *)

let a = utf16_of_utf8 "ABC" in
let b = UTF16.copy a in
assert (a = b && a != b);;

(* UTF16/32.append *)

let a = utf16_of_utf8 "ABC" in
let b = utf16_of_utf8 "DEF" in
assert (UTF16.append a b = utf16_of_utf8 "ABCDEF");;

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
