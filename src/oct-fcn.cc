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

#include "error.h"
#include "oct-fcn.h"

octave_function::octave_function (octave_function *new_rep)
  : rep (new_rep)
{
  rep->count = 1;
}

octave_function::~octave_function (void)
{
#if defined (MDEBUG)
  cerr << "~octave_function: rep: " << rep
       << " rep->count: " << rep->count << "\n";
#endif

  if (rep && --rep->count == 0)
    {
      delete rep;
      rep = 0;
    }
}

octave_function *
octave_function::clone (void)
{
  panic_impossible ();
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
