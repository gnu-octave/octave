////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 1996-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun.h"
#include "ov.h"
#include "ovl.h"
#include "pt-arg-list.h"
#include "pt-eval.h"
#include "pt-exp.h"
#include "pt-mat.h"
#include "pt-tm-const.h"
#include "variables.h"

#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"

OCTAVE_BEGIN_NAMESPACE(octave)

octave_value
tree_matrix::evaluate (tree_evaluator& tw, int)
{
  tm_const tmp (*this, tw);

  return tmp.concat (tw.string_fill_char ());
}

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

      if (c1 == "cell" || c2 == "cell")
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
tree_matrix::dup (symbol_scope& scope) const
{
  tree_matrix *new_matrix = new tree_matrix (nullptr, line (), column ());

  new_matrix->copy_base (*this, scope);

  return new_matrix;
}

OCTAVE_END_NAMESPACE(octave)
