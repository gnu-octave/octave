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

#if defined (__GNUG__)
#pragma implementation
#endif

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream.h>

#include "oct-obj.h"
#include "pt-const.h"
#include "pt-walk.h"

// We are likely to have a lot of tree_constant objects to allocate,
// so make the grow_size large.
octave_allocator
tree_constant::allocator (sizeof (tree_constant), 1024);

void
tree_constant::print (ostream& os, bool pr_as_read_syntax, bool pr_orig_text)
{
  if (pr_orig_text && ! orig_text.empty ())
    os << orig_text;
  else
    val.print (os, pr_as_read_syntax);
}

octave_value
tree_constant::eval (bool print_result)
{
  if (print_result)
    val.print ();

  return val;
}

octave_value_list
tree_constant::eval (bool, int, const octave_value_list& idx)
{
  return (idx.length () > 0) ? index (idx) : val;
}

void
tree_constant::accept (tree_walker& tw)
{
  tw.visit_constant (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
