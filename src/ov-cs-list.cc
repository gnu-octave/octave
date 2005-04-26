/*

Copyright (C) 2002 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "lo-sstream.h"
#include "lo-utils.h"

#include "defun.h"
#include "error.h"
#include "ov-cs-list.h"
#include "unwind-prot.h"

DEFINE_OCTAVE_ALLOCATOR (octave_cs_list);

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_cs_list, "cs-list", "cs-list");

octave_cs_list::octave_cs_list (const Cell& c)
  : octave_base_value (), lst ()
{
  octave_idx_type n = c.length ();

  lst.resize (n);

  for (octave_idx_type i = 0; i < n; i++)
    lst(i) = c(i);
}

void
octave_cs_list::print (std::ostream& os, bool) const
{
  print_raw (os);
}

void
octave_cs_list::print_raw (std::ostream& os, bool) const
{
  unwind_protect::begin_frame ("octave_cs_list_print");

  octave_idx_type n = lst.length ();

  if (n > 0)
    {
      indent (os);
      os << "(,";
      newline (os);

      increment_indent_level ();

      for (octave_idx_type i = 0; i < n; i++)
	{
	  OSSTREAM buf;
	  buf << "[" << i+1 << "]" << OSSTREAM_ENDS;

	  octave_value val = lst(i);

	  val.print_with_name (os, OSSTREAM_STR (buf));

	  OSSTREAM_FREEZE (buf);
	}

      decrement_indent_level ();

      indent (os);
      os << ",)";
    }
  else
    os << "(,,)";

  newline (os);

  unwind_protect::run_frame ("octave_cs_list_print");
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
