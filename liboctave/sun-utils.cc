/*

Copyright (C) 1996 John W. Eaton

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

#include <cassert>

// I think that this is really only needed if linking to Fortran
// compiled libraries on a Sun.  It also seems to be needed on
// Linux/ELF systems with g77.  It should never be called.

extern "C"
{
#if defined (sun)
  int MAIN_ (void)
    {
      assert (0);
      return 0;
    }
#elif defined (linux) && defined (__ELF__)
  int MAIN__ (void)
    {
      assert (0);
      return 0;
    }
#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

