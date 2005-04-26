/*

Copyright (C) 2003 John W. Eaton

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

#include "error.h"
#include "oct-obj.h"
#include "ov-fcn-handle.h"
#include "pt-fcn-handle.h"
#include "pager.h"
#include "pt-walk.h"

void
tree_fcn_handle::print (std::ostream& os, bool pr_as_read_syntax,
			bool pr_orig_text)
{
  print_raw (os, pr_as_read_syntax, pr_orig_text);
}

void
tree_fcn_handle::print_raw (std::ostream& os, bool pr_as_read_syntax,
			    bool pr_orig_text) 
{
  os << ((pr_as_read_syntax || pr_orig_text) ? "@" : "") << nm;
}

octave_value
tree_fcn_handle::rvalue (void)
{
  MAYBE_DO_BREAKPOINT;

  return make_fcn_handle (nm);
}


octave_value_list
tree_fcn_handle::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for function handle expression");
  else
    retval = rvalue ();

  return retval;
}

void
tree_fcn_handle::accept (tree_walker& tw)
{
  tw.visit_fcn_handle (*this);
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
