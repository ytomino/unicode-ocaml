very very simple unicode library for Objective-Caml
===================================================

What's this?
------------

This library has simple purpose for converting between UTF-8/16/32.

Prerequisites
-------------

OCaml >= 4.11
 https://ocaml.org/

How to make
-----------

Install
+++++++

::

 make install PREFIX=/usr/local

Specify your preferred directory to ``PREFIX``.
The libraries would be installed into ``$PREFIX/lib/ocaml`` (default is
``ocamlc -where``).

Uninstall
+++++++++

::

 make uninstall PREFIX=/usr/local

Build examples
++++++++++++++

::

 make -C examples

Introduction
------------

Types
+++++

================ ==============================================================
``Uint32.t``     unsigned 32bit int
``utf8_char``    8bit char ( = ``char`` )
``utf16_char``   16bit char ( = ``int`` )
``utf32_char``   32bit char ( = ``Uint32.t`` )
``utf8_string``  string of UTF-8 ( = ``string`` )
``utf16_string`` string of UTF-16 ( = ``Bigarray.Array1.t`` )
``utf32_string`` string of UTF-32 ( = ``Bigarray.Array1.t`` )
================ ==============================================================

And ``Uchar.t`` of Stdlib is used to represent a 31bit code point.

Functions
+++++++++

===================== =========================================================
``utfXX_sequence``    length of single code point of a leading element
``utfXX_is_trailing`` whether an element is trailing or not
``utfXX_decode``      decoding single code point
``utfXX_encode``      encoding single code point
``utfXX_lead``        the first index of the contained code point from an index
``utfXX_rear``        the last index of the contained code point from an index
``utfXX_get_code``    decoding single code point from a string
``utfXX_set_code``    encoding single code point into a string
``utfXX_of_utfXX``    new string converted from a string
===================== =========================================================

Limitations
+++++++++++

U+7FFFFFFF is the maximum that this library can handle correctly in 32bit
environments.
