open Unicode;;

let exn = Failure "test_elt.ml";;

(* sequence *)

let utf16_sequence_should_fail item = (
	try
		let _: int = utf16_sequence ~illegal_sequence:exn item in
		false
	with Failure _ -> true
);;

for i = 0 to 0xd7 do
	assert (utf16_sequence ~illegal_sequence:exn (i * 0x100) = 1)
done;;
assert (utf16_sequence ~illegal_sequence:exn 0xd7ff = 1);;
assert (utf16_sequence ~illegal_sequence:exn 0xd800 = 2);;
assert (utf16_sequence ~illegal_sequence:exn 0xd955 = 2);;
assert (utf16_sequence ~illegal_sequence:exn 0xdaaa = 2);;
assert (utf16_sequence ~illegal_sequence:exn 0xdbff = 2);;
assert (utf16_sequence_should_fail 0xdc00);;
assert (utf16_sequence_should_fail 0xdfff);;
assert (utf16_sequence ~illegal_sequence:exn 0xe000 = 1);;
for i = 0xe0 to 0xff do
	assert (utf16_sequence (i * 0x100 lor 0xff) = 1)
done;;

assert (utf16_sequence ~illegal_sequence:exn 0x10000 = 1);; (* masked *)
assert (utf16_sequence ~illegal_sequence:exn 0x1d800 = 2);; (* masked *)
assert (utf16_sequence_should_fail 0x1dc00);; (* masked *)
assert (utf16_sequence ~illegal_sequence:exn (lnot 0xffff) = 1);; (* masked *)

(* is_trailing *)

for i = 0 to 7 do
	assert (not (UTF8.is_trailing (char_of_int (i * 0x10))))
done;;
assert (not (UTF8.is_trailing (char_of_int 0x7f)));;
assert (UTF8.is_trailing (char_of_int 0x80));;
assert (UTF8.is_trailing (char_of_int 0x95));;
assert (UTF8.is_trailing (char_of_int 0xaa));;
assert (UTF8.is_trailing (char_of_int 0xbf));;
assert (not (UTF8.is_trailing (char_of_int 0xc0)));;
for i = 0xc to 0xf do
	assert (not (UTF8.is_trailing (char_of_int (i * 0x10 lor 0xf))));
done;;

for i = 0 to 0xdb do
	assert (not (UTF16.is_trailing (i * 0x100)))
done;;
assert (not (UTF16.is_trailing 0xdbff));;
assert (UTF16.is_trailing 0xdc00);;
assert (UTF16.is_trailing 0xdd55);;
assert (UTF16.is_trailing 0xdeaa);;
assert (UTF16.is_trailing 0xdfff);;
assert (not (UTF16.is_trailing 0xe000));;
for i = 0xe0 to 0xff do
	assert (not (UTF16.is_trailing (i * 0x100 lor 0xff)))
done;;
assert (UTF16.is_trailing (0xdc00 lor 0x10000));; (* masked *)
assert (UTF16.is_trailing (0xdfff lor lnot 0xffff));; (* masked *)

assert (not (UTF32.is_trailing (Uint32.of_int32 0l)));
assert (not (UTF32.is_trailing (Uint32.of_int32 0xffffffffl)));

(* report *)

prerr_endline "ok";;
