let data = "あいうえお";;
let utf16 = Unicode.utf16_of_utf8 data;;
let utf32 = Unicode.utf32_of_utf8 data;;
assert (Unicode.utf8_of_utf16 utf16 = data);
assert (Unicode.utf8_of_utf32 utf32 = data);
assert (Unicode.utf16_of_utf32 utf32 = utf16);
assert (Unicode.utf32_of_utf16 utf16 = utf32);
