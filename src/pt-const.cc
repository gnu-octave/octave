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

#include <cctype>
#include <cstring>

#include <string>

#include <fstream.h>
#include <iostream.h>

#include <SLList.h>

#include "Array-flags.h"

#include "mx-base.h"
#include "Range.h"
#include "str-vec.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "idx-vector.h"
#include "mappers.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "pager.h"
#include "pr-output.h"
#include "sysdep.h"
#include "pt-const.h"
#include "pt-walk.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// We are likely to have a lot of tree_constant objects to allocate,
// so make the grow_size large.
octave_allocator
tree_constant::allocator (sizeof (tree_constant), 1024);

Octave_map
tree_constant::map_value (void) const
{
  return val.map_value ();
}

void
tree_constant::print (void)
{
}

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
  octave_value_list retval;

  if (idx.length () >  0)
    retval (0) = index (idx);
  else
    retval (0) = val;

  return retval;
}

octave_value
tree_constant::lookup_map_element (const string&, bool, bool)
{
  octave_value retval;
  error ("tree_constant::lookup_map_element() not implemented");
  return retval;
}

octave_value
tree_constant::lookup_map_element (SLList<string>&, bool, bool)
{
  octave_value retval;
  error ("tree_constant::lookup_map_element() not implemented");
  return retval;
}

void
tree_constant::stash_original_text (const string& s)
{
  orig_text = s;
}

string
tree_constant::original_text (void) const
{
  return orig_text;
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
