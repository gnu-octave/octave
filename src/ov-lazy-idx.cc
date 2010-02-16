
/*

Copyright (C) 2010 VZLU Prague

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "ov-lazy-idx.h"
#include "ops.h"
#include "ov-scalar.h"
#include "ls-oct-ascii.h"
#include "ls-oct-binary.h"

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_lazy_index, "lazy_index", "double");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_lazy_index&);

  return v.full_value ().clone ();
}

octave_base_value::type_conv_info
octave_lazy_index::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
                                            octave_matrix::static_type_id ());
}

octave_base_value *
octave_lazy_index::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  switch (index.length (0))
    {
    case 1:
      retval = new octave_scalar (static_cast<double> (index(0) + 1));
      break;

    case 0:
      retval = new octave_matrix (NDArray (index.orig_dimensions ()));
      break;

    default:
      break;
    }

  return retval;
}

static const std::string value_save_tag ("index_value");

bool octave_lazy_index::save_ascii (std::ostream& os)
{
  return save_ascii_data (os, make_value (), value_save_tag, false, 0);
}

bool octave_lazy_index::load_ascii (std::istream& is)
{
  bool dummy;

  std::string nm = read_ascii_data (is, std::string (), dummy, value, 0);

  if (nm != value_save_tag)
    error ("lazy_index: corrupted data on load");
  else
    index = value.index_vector ();

  return ! error_state;
}


bool octave_lazy_index::save_binary (std::ostream& os, bool& save_as_floats)
{
  return save_binary_data (os, make_value (), value_save_tag, false, 0, save_as_floats);
}

bool octave_lazy_index::load_binary (std::istream& is, bool swap, 
                                     oct_mach_info::float_format fmt)
{
  bool dummy;
  std::string doc;

  std::string nm = read_binary_data (is, swap, fmt, std::string (), 
                                     dummy, value, doc);

  if (nm != value_save_tag)
    error ("lazy_index: corrupted data on load");
  else
    index = value.index_vector ();

  return ! error_state;
}
