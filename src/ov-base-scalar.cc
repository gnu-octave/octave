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

#include "ov-base.h"
#include "ov-base-scalar.h"
#include "pr-output.h"

template <class ST>
void
octave_base_scalar<ST>::print (ostream& os, bool pr_as_read_syntax) const
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

template <class ST>
void
octave_base_scalar<ST>::print_raw (ostream& os, bool pr_as_read_syntax) const
{
  indent (os);
  octave_print_internal (os, scalar, pr_as_read_syntax);
}

template <class ST>
bool
octave_base_scalar<ST>::print_name_tag (ostream& os, const string& name) const
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
