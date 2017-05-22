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

#include "error.h"
#include "ovl.h"
#include "ov-fcn.h"


octave_base_value *
octave_function::clone (void) const
{
  panic_impossible ();
  return 0;
}

octave_base_value *
octave_function::empty_clone (void) const
{
  panic_impossible ();
  return 0;
}

octave_value_list
octave_function::subsref (const std::string& type,
                          const std::list<octave_value_list>& idx,
                          int nargout)
{
  octave_value_list retval;

  switch (type[0])
    {
    case '(':
      {
        int tmp_nargout = (type.length () > 1 && nargout == 0) ? 1 : nargout;

        retval = call (tmp_nargout, idx.front ());
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

  // FIXME: perhaps there should be an
  // octave_value_list::next_subsref member function?  See also
  // octave_user_function::subsref.
  //
  // FIXME: Note that if a function call returns multiple
  // values, and there is further indexing to perform, then we are
  // ignoring all but the first value.  Is this really what we want to
  // do?  If it is not, then what should happen for stat("file").size,
  // for exmaple?

  if (idx.size () > 1)
    retval = retval(0).next_subsref (nargout, type, idx);

  return retval;
}
