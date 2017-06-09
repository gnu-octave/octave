/*

Copyright (C) 1996-2017 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <iostream>

#include "oct-locbuf.h"
#include "quit.h"

#include "data.h"
#include "defun.h"
#include "error.h"
#include "errwarn.h"
#include "oct-map.h"
#include "ovl.h"
#include "pt-arg-list.h"
#include "pt-bp.h"
#include "pt-exp.h"
#include "pt-mat.h"
#include "pt-tm-const.h"
#include "pt-walk.h"
#include "utils.h"
#include "ov.h"
#include "variables.h"

#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

// The character to fill with when creating string arrays.
char Vstring_fill_char = ' ';

namespace octave
{
  std::string
  get_concat_class (const std::string& c1, const std::string& c2)
  {
    std::string retval = octave_base_value::static_class_name ();

    if (c1 == c2)
      retval = c1;
    else if (c1.empty ())
      retval = c2;
    else if (c2.empty ())
      retval = c1;
    else if (c1 == "class" || c2 == "class")
      retval = "class";
    else
      {
        bool c1_is_int = (c1 == "int8" || c1 == "uint8"
                          || c1 == "int16" || c1 == "uint16"
                          || c1 == "int32" || c1 == "uint32"
                          || c1 == "int64" || c1 == "uint64");
        bool c2_is_int = (c2 == "int8" || c2 == "uint8"
                          || c2 == "int16" || c2 == "uint16"
                          || c2 == "int32" || c2 == "uint32"
                          || c2 == "int64" || c2 == "uint64");

        bool c1_is_char = (c1 == "char");
        bool c2_is_char = (c2 == "char");

        bool c1_is_double = (c1 == "double");
        bool c2_is_double = (c2 == "double");

        bool c1_is_single = (c1 == "single");
        bool c2_is_single = (c2 == "single");

        bool c1_is_logical = (c1 == "logical");
        bool c2_is_logical = (c2 == "logical");

        bool c1_is_built_in_type
          = (c1_is_int || c1_is_char || c1_is_double || c1_is_single
             || c1_is_logical);

        bool c2_is_built_in_type
          = (c2_is_int || c2_is_char ||  c2_is_double || c2_is_single
             || c2_is_logical);

        // Order is important here...

        if (c1 == "struct" && c2 == c1)
          retval = c1;
        else if (c1 == "cell" || c2 == "cell")
          retval = "cell";
        else if (c1_is_char && c2_is_built_in_type)
          retval = c1;
        else if (c2_is_char && c1_is_built_in_type)
          retval = c2;
        else if (c1_is_int && c2_is_built_in_type)
          retval = c1;
        else if (c2_is_int && c1_is_built_in_type)
          retval = c2;
        else if (c1_is_single && c2_is_built_in_type)
          retval = c1;
        else if (c2_is_single && c1_is_built_in_type)
          retval = c2;
        else if (c1_is_double && c2_is_built_in_type)
          retval = c1;
        else if (c2_is_double && c1_is_built_in_type)
          retval = c2;
        else if (c1_is_logical && c2_is_logical)
          retval = c1;
      }

    return retval;
  }

  void
  maybe_warn_string_concat (bool all_dq_strings_p, bool all_sq_strings_p)
  {
    if (! (all_dq_strings_p || all_sq_strings_p))
      warning_with_id ("Octave:mixed-string-concat",
                       "concatenation of different character string types may have unintended consequences");
  }

  tree_expression *
  tree_matrix::dup (symbol_table::scope_id scope,
                    symbol_table::context_id context) const
  {
    tree_matrix *new_matrix = new tree_matrix (0, line (), column ());

    new_matrix->copy_base (*this, scope, context);

    return new_matrix;
  }
}


/*
## test concatenation with all zero matrices
%!assert ([ "" 65*ones(1,10) ], "AAAAAAAAAA")
%!assert ([ 65*ones(1,10) "" ], "AAAAAAAAAA")

%!test
%! c = {"foo"; "bar"; "bazoloa"};
%! assert ([c; "a"; "bc"; "def"], {"foo"; "bar"; "bazoloa"; "a"; "bc"; "def"});

%!assert (class ([int64(1), int64(1)]), "int64")
%!assert (class ([int64(1), int32(1)]), "int64")
%!assert (class ([int64(1), int16(1)]), "int64")
%!assert (class ([int64(1), int8(1)]), "int64")
%!assert (class ([int64(1), uint64(1)]), "int64")
%!assert (class ([int64(1), uint32(1)]), "int64")
%!assert (class ([int64(1), uint16(1)]), "int64")
%!assert (class ([int64(1), uint8(1)]), "int64")
%!assert (class ([int64(1), single(1)]), "int64")
%!assert (class ([int64(1), double(1)]), "int64")
%!assert (class ([int64(1), cell(1)]), "cell")
%!assert (class ([int64(1), true]), "int64")
%!assert (class ([int64(1), "a"]), "char")

%!assert (class ([int32(1), int64(1)]), "int32")
%!assert (class ([int32(1), int32(1)]), "int32")
%!assert (class ([int32(1), int16(1)]), "int32")
%!assert (class ([int32(1), int8(1)]), "int32")
%!assert (class ([int32(1), uint64(1)]), "int32")
%!assert (class ([int32(1), uint32(1)]), "int32")
%!assert (class ([int32(1), uint16(1)]), "int32")
%!assert (class ([int32(1), uint8(1)]), "int32")
%!assert (class ([int32(1), single(1)]), "int32")
%!assert (class ([int32(1), double(1)]), "int32")
%!assert (class ([int32(1), cell(1)]), "cell")
%!assert (class ([int32(1), true]), "int32")
%!assert (class ([int32(1), "a"]), "char")

%!assert (class ([int16(1), int64(1)]), "int16")
%!assert (class ([int16(1), int32(1)]), "int16")
%!assert (class ([int16(1), int16(1)]), "int16")
%!assert (class ([int16(1), int8(1)]), "int16")
%!assert (class ([int16(1), uint64(1)]), "int16")
%!assert (class ([int16(1), uint32(1)]), "int16")
%!assert (class ([int16(1), uint16(1)]), "int16")
%!assert (class ([int16(1), uint8(1)]), "int16")
%!assert (class ([int16(1), single(1)]), "int16")
%!assert (class ([int16(1), double(1)]), "int16")
%!assert (class ([int16(1), cell(1)]), "cell")
%!assert (class ([int16(1), true]), "int16")
%!assert (class ([int16(1), "a"]), "char")

%!assert (class ([int8(1), int64(1)]), "int8")
%!assert (class ([int8(1), int32(1)]), "int8")
%!assert (class ([int8(1), int16(1)]), "int8")
%!assert (class ([int8(1), int8(1)]), "int8")
%!assert (class ([int8(1), uint64(1)]), "int8")
%!assert (class ([int8(1), uint32(1)]), "int8")
%!assert (class ([int8(1), uint16(1)]), "int8")
%!assert (class ([int8(1), uint8(1)]), "int8")
%!assert (class ([int8(1), single(1)]), "int8")
%!assert (class ([int8(1), double(1)]), "int8")
%!assert (class ([int8(1), cell(1)]), "cell")
%!assert (class ([int8(1), true]), "int8")
%!assert (class ([int8(1), "a"]), "char")

%!assert (class ([uint64(1), int64(1)]), "uint64")
%!assert (class ([uint64(1), int32(1)]), "uint64")
%!assert (class ([uint64(1), int16(1)]), "uint64")
%!assert (class ([uint64(1), int8(1)]), "uint64")
%!assert (class ([uint64(1), uint64(1)]), "uint64")
%!assert (class ([uint64(1), uint32(1)]), "uint64")
%!assert (class ([uint64(1), uint16(1)]), "uint64")
%!assert (class ([uint64(1), uint8(1)]), "uint64")
%!assert (class ([uint64(1), single(1)]), "uint64")
%!assert (class ([uint64(1), double(1)]), "uint64")
%!assert (class ([uint64(1), cell(1)]), "cell")
%!assert (class ([uint64(1), true]), "uint64")
%!assert (class ([uint64(1), "a"]), "char")

%!assert (class ([uint32(1), int64(1)]), "uint32")
%!assert (class ([uint32(1), int32(1)]), "uint32")
%!assert (class ([uint32(1), int16(1)]), "uint32")
%!assert (class ([uint32(1), int8(1)]), "uint32")
%!assert (class ([uint32(1), uint64(1)]), "uint32")
%!assert (class ([uint32(1), uint32(1)]), "uint32")
%!assert (class ([uint32(1), uint16(1)]), "uint32")
%!assert (class ([uint32(1), uint8(1)]), "uint32")
%!assert (class ([uint32(1), single(1)]), "uint32")
%!assert (class ([uint32(1), double(1)]), "uint32")
%!assert (class ([uint32(1), cell(1)]), "cell")
%!assert (class ([uint32(1), true]), "uint32")
%!assert (class ([uint32(1), "a"]), "char")

%!assert (class ([uint16(1), int64(1)]), "uint16")
%!assert (class ([uint16(1), int32(1)]), "uint16")
%!assert (class ([uint16(1), int16(1)]), "uint16")
%!assert (class ([uint16(1), int8(1)]), "uint16")
%!assert (class ([uint16(1), uint64(1)]), "uint16")
%!assert (class ([uint16(1), uint32(1)]), "uint16")
%!assert (class ([uint16(1), uint16(1)]), "uint16")
%!assert (class ([uint16(1), uint8(1)]), "uint16")
%!assert (class ([uint16(1), single(1)]), "uint16")
%!assert (class ([uint16(1), double(1)]), "uint16")
%!assert (class ([uint16(1), cell(1)]), "cell")
%!assert (class ([uint16(1), true]), "uint16")
%!assert (class ([uint16(1), "a"]), "char")

%!assert (class ([uint8(1), int64(1)]), "uint8")
%!assert (class ([uint8(1), int32(1)]), "uint8")
%!assert (class ([uint8(1), int16(1)]), "uint8")
%!assert (class ([uint8(1), int8(1)]), "uint8")
%!assert (class ([uint8(1), uint64(1)]), "uint8")
%!assert (class ([uint8(1), uint32(1)]), "uint8")
%!assert (class ([uint8(1), uint16(1)]), "uint8")
%!assert (class ([uint8(1), uint8(1)]), "uint8")
%!assert (class ([uint8(1), single(1)]), "uint8")
%!assert (class ([uint8(1), double(1)]), "uint8")
%!assert (class ([uint8(1), cell(1)]), "cell")
%!assert (class ([uint8(1), true]), "uint8")
%!assert (class ([uint8(1), "a"]), "char")

%!assert (class ([single(1), int64(1)]), "int64")
%!assert (class ([single(1), int32(1)]), "int32")
%!assert (class ([single(1), int16(1)]), "int16")
%!assert (class ([single(1), int8(1)]), "int8")
%!assert (class ([single(1), uint64(1)]), "uint64")
%!assert (class ([single(1), uint32(1)]), "uint32")
%!assert (class ([single(1), uint16(1)]), "uint16")
%!assert (class ([single(1), uint8(1)]), "uint8")
%!assert (class ([single(1), single(1)]), "single")
%!assert (class ([single(1), double(1)]), "single")
%!assert (class ([single(1), cell(1)]), "cell")
%!assert (class ([single(1), true]), "single")
%!assert (class ([single(1), "a"]), "char")

%!assert (class ([double(1), int64(1)]), "int64")
%!assert (class ([double(1), int32(1)]), "int32")
%!assert (class ([double(1), int16(1)]), "int16")
%!assert (class ([double(1), int8(1)]), "int8")
%!assert (class ([double(1), uint64(1)]), "uint64")
%!assert (class ([double(1), uint32(1)]), "uint32")
%!assert (class ([double(1), uint16(1)]), "uint16")
%!assert (class ([double(1), uint8(1)]), "uint8")
%!assert (class ([double(1), single(1)]), "single")
%!assert (class ([double(1), double(1)]), "double")
%!assert (class ([double(1), cell(1)]), "cell")
%!assert (class ([double(1), true]), "double")
%!assert (class ([double(1), "a"]), "char")

%!assert (class ([cell(1), int64(1)]), "cell")
%!assert (class ([cell(1), int32(1)]), "cell")
%!assert (class ([cell(1), int16(1)]), "cell")
%!assert (class ([cell(1), int8(1)]), "cell")
%!assert (class ([cell(1), uint64(1)]), "cell")
%!assert (class ([cell(1), uint32(1)]), "cell")
%!assert (class ([cell(1), uint16(1)]), "cell")
%!assert (class ([cell(1), uint8(1)]), "cell")
%!assert (class ([cell(1), single(1)]), "cell")
%!assert (class ([cell(1), double(1)]), "cell")
%!assert (class ([cell(1), cell(1)]), "cell")
%!assert (class ([cell(1), true]), "cell")
%!assert (class ([cell(1), "a"]), "cell")

%!assert (class ([true, int64(1)]), "int64")
%!assert (class ([true, int32(1)]), "int32")
%!assert (class ([true, int16(1)]), "int16")
%!assert (class ([true, int8(1)]), "int8")
%!assert (class ([true, uint64(1)]), "uint64")
%!assert (class ([true, uint32(1)]), "uint32")
%!assert (class ([true, uint16(1)]), "uint16")
%!assert (class ([true, uint8(1)]), "uint8")
%!assert (class ([true, single(1)]), "single")
%!assert (class ([true, double(1)]), "double")
%!assert (class ([true, cell(1)]), "cell")
%!assert (class ([true, true]), "logical")
%!assert (class ([true, "a"]), "char")

%!assert (class (["a", int64(1)]), "char")
%!assert (class (["a", int32(1)]), "char")
%!assert (class (["a", int16(1)]), "char")
%!assert (class (["a", int8(1)]), "char")
%!assert (class (["a", int64(1)]), "char")
%!assert (class (["a", int32(1)]), "char")
%!assert (class (["a", int16(1)]), "char")
%!assert (class (["a", int8(1)]), "char")
%!assert (class (["a", single(1)]), "char")
%!assert (class (["a", double(1)]), "char")
%!assert (class (["a", cell(1)]), "cell")
%!assert (class (["a", true]), "char")
%!assert (class (["a", "a"]), "char")

%!assert (class ([cell(1), struct("foo", "bar")]), "cell")
%!error [struct("foo", "bar"), cell(1)]

%!test <*39041> assert (class ([cell(0), struct()]), "cell")
%!test <*39041> assert (class ([cell(0), struct()]), "cell")
%!test <51086> assert (class ([struct(), cell(0)]), "struct")

%!assert ([,1], 1)
%!assert ([1,], 1)
%!assert ([,1,], 1)
%!assert ([,1,;;], 1)
%!assert ([,1,;,;], 1)

%!assert ([1,1], ones (1, 2))
%!assert ([,1,1], ones (1, 2))
%!assert ([1,1,], ones (1, 2))
%!assert ([,1,1,], ones (1, 2))
%!assert ([,1,1,;;], ones (1, 2))
%!assert ([,1,1,;,;], ones (1, 2))
%!assert ([,;,1,1], ones (1, 2))

%!assert ([1;1], ones (2, 1))
%!assert ([1,;1], ones (2, 1))
%!assert ([1,;,;1], ones (2, 1))

%!error eval ("[,,]")
%!error eval ("[,,;,]")
%!error eval ("[,;,,;,]")

%!assert (isnull ([,]))
%!assert (isnull ([;]))
%!assert (isnull ([;;]))
%!assert (isnull ([;,;]))
%!assert (isnull ([,;,;,]))
*/

DEFUN (string_fill_char, args, nargout,
       doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} string_fill_char ()
@deftypefnx {} {@var{old_val} =} string_fill_char (@var{new_val})
@deftypefnx {} {} string_fill_char (@var{new_val}, "local")
Query or set the internal variable used to pad all rows of a character
matrix to the same length.

The value must be a single character and the default is @qcode{" "} (a
single space).  For example:

@example
@group
string_fill_char ("X");
[ "these"; "are"; "strings" ]
      @result{}  "theseXX"
          "areXXXX"
          "strings"
@end group
@end example

When called from inside a function with the @qcode{"local"} option, the
variable is changed locally for the function and any subroutines it calls.
The original variable value is restored when exiting the function.
@end deftypefn */)
{
  return SET_INTERNAL_VARIABLE (string_fill_char);
}

/*
## string_fill_char() function call must be outside of %!test block
## due to the way a %!test block is wrapped inside a function
%!shared orig_val, old_val
%! orig_val = string_fill_char ();
%! old_val  = string_fill_char ("X");
%!test
%! assert (orig_val, old_val);
%! assert (string_fill_char (), "X");
%! assert (["these"; "are"; "strings"], ["theseXX"; "areXXXX"; "strings"]);
%! string_fill_char (orig_val);
%! assert (string_fill_char (), orig_val);

%!assert ( [ [], {1} ], {1} )

%!error (string_fill_char (1, 2))
*/
