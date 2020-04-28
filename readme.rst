very very simple unicode library for Objective-Caml
===================================================

What's this?
------------

This library has simple purpose for converting between UTF-8/16/32.

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

================ ===================================================================
``ucs4``         unsigned 31-bit int representing a code-point ( = ``int32_t`` )
``utf8_char``    8-bit char ( = ``char`` )
``utf16_char``   16-bit char ( = ``int`` )
``utf32_char``   31-bit char ( = ``ucs4`` )
``utf8_string``  string of UTF-8 ( = ``string`` )
``utf16_string`` string of UTF-16 ( = ``Bigarray.Array1.t`` )
``utf32_string`` string of UTF-32 ( = ``Bigarray.Array1.t`` )
================ ===================================================================

Functions
+++++++++

================== ======================================================
``utf_X_sequence`` length of single code-point of the leading element
``utf_X_get_code`` decoding single code-point from the string
``utf_X_lead``     getting index of the leading element from any index
``utf_X_encode``   encoding single code-point
``utf_X_set_code`` encoding single code-point to the string
``utf_X_of_utf_X`` converting all elements of the string
================== ======================================================

Policy for handling illegal sequence
++++++++++++++++++++++++++++++++++++

On illegal sequence that is shortage/too many trailing elements in UTF-8/16 or
surrogate pair in UTF-8/32,
if optional parameter ?illegal_sequence is given, the function raises it.

Otherwise,

======== ==================================================
shortage L3 T N... will be separated as [L3 T "0"] [N...]
too many L2 T T N... will be separated as [L2 T] [T] [N...]
======== ==================================================

(LX means leading element of sequence having X elements, T means trailing
element, N means next sequence)

and surrogate pair will be taken as normal code-point.

License
-------

**license of unicode-ocaml** ::

 Copyright 2008-2020 YT. All rights reserved.
 
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
