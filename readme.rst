very very simple unicode library for Objective-Caml
===================================================

What's this?
------------

This library has simple purpose for converting between UTF-8/16/32.

Prerequisites
-------------

OCaml >= 4.08
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
And set ``$PREFIX/lib/ocaml/stublibs`` to ``CAML_LD_LIBRARY_PATH``.

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

Policy for handling illegal sequence
++++++++++++++++++++++++++++++++++++

When some illegal sequence is found,

If the optional parameter ``?illegal_sequence`` is given, the functions will
raise it.

Otherwise, the functions will continue as if the missing element were present.

======== ==================================================
shortage L3 T N... will be separated as [L3 T "0"] [N...]
too many L2 T T N... will be separated as [L2 T] [T] [N...]
======== ==================================================

(LX means some leading element of sequence having X elements, T means some
trailing element, N means the start of the next sequence.)

And such as overlong encoded code points or halfs of surrogate pair will be
taken as normal code point.

Limitations
+++++++++++

U+7FFFFFFF is the maximum that this library can handle correctly in 32bit
environments.

License
-------

**license of unicode-ocaml** ::

 Copyright 2008-2022 YT. All rights reserved.
 
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 
 THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
