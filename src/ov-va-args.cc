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

#include "error.h"
#include "pr-output.h"
#include "oct-obj.h"
#include "ov-va-args.h"

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_all_va_args, "va-arg", "va-arg");

void
octave_all_va_args::print (std::ostream& os, bool) const
{
  indent (os);
  print (os);
}

void
octave_all_va_args::print_raw (std::ostream& os, bool) const
{
  os << "all_va_args";
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
