/*

Copyright (C) 1996, 1997 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "oct-obj.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-file.h"
#include "ov-scalar.h"
#include "unwind-prot.h"

DEFINE_OCTAVE_ALLOCATOR (octave_file);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_file, "file", "file");

static octave_value *
default_numeric_conversion_function (const octave_value& a)
{
  CAST_CONV_ARG (const octave_file&);

  return new octave_scalar (v.stream_number ());
}

type_conv_fcn
octave_file::numeric_conversion_function (void) const
{
  return default_numeric_conversion_function;
}

void
octave_file::print (std::ostream& os, bool) const
{
  print_raw (os);
  newline (os);
}

void
octave_file::print_raw (std::ostream& os, bool) const
{
  indent (os); os << "{"; newline (os);

  increment_indent_level ();

  indent (os);
  os << "id = " << number;
  newline (os);

  indent (os);
  os << "name = " << stream.name ();
  newline (os);

  indent (os);
  os << "mode = " << octave_stream::mode_as_string (stream.mode ());
  newline (os);

  indent (os);
  os << "arch = "
     << oct_mach_info::float_format_as_string (stream.float_format ());
  newline (os);

  indent (os);
  os << "status = " << (stream.is_open () ? "open" : "closed");
  newline (os);

  decrement_indent_level ();

  indent (os); os << "}";
}

bool
octave_file::print_name_tag (std::ostream& os, const std::string& name) const
{
  indent (os);
  os << name << " =";
  newline (os);
  return false;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
