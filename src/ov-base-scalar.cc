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
#include "ov-base.h"
#include "ov-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-base-scalar.h"
#include "pr-output.h"

template <class ST>
octave_value
octave_base_scalar<ST>::subsref (const std::string& type,
				 const std::list<octave_value_list>& idx)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      retval = do_index_op (idx.front ());
      break;

    case '{':
    case '.':
      {
	std::string nm = type_name ();
	error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval.next_subsref (type, idx);
}

template <class ST>
octave_value
octave_base_scalar<ST>::subsasgn (const std::string& type,
				  const std::list<octave_value_list>& idx,
				  const octave_value& rhs)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      {
	if (type.length () == 1)
	  {
	    if (idx.front().valid_scalar_indices ()
		&& rhs.is_scalar_type ()
		&& rhs.is_numeric_type ())
	      retval = rhs;
	    else
	      retval = numeric_assign (type, idx, rhs);
	  }
	else
	  {
	    std::string nm = type_name ();
	    error ("in indexed assignment of %s, last rhs index must be ()",
		   nm.c_str ());
	  }
      }
      break;

    case '{':
    case '.':
      {
	std::string nm = type_name ();
	error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
      }
      break;

    default:
      panic_impossible ();
    }

  return retval;
}

template <class ST>
void
octave_base_scalar<ST>::print (std::ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

template <class ST>
void
octave_base_scalar<ST>::print_raw (std::ostream& os,
				   bool pr_as_read_syntax) const
{
  indent (os);
  octave_print_internal (os, scalar, pr_as_read_syntax);
}

template <class ST>
bool
octave_base_scalar<ST>::print_name_tag (std::ostream& os,
					const std::string& name) const
{
  indent (os);
  os << name << " = ";
  return false;    
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
