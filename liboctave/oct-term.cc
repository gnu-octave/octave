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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "oct-term.h"

// For now, use the variables from readline.  It already handles
// SIGWINCH, so these values have a good chance of being correct even
// if the window changes size (they will be wrong if, for example, the
// luser changes the window size while the pager is running, and the
// signal is handled by the pager instead of us.

int
terminal_columns (void)
{
#if defined (USE_READLINE)
  extern int screenwidth;
  return screenwidth > 0 ? screenwidth : 80;
#else
  // XXX FIXME XXX
  return 80;
#endif
}

int
terminal_rows (void)
{
#if defined (USE_READLINE)
  extern int screenheight;
  return screenheight > 0 ? screenheight : 24;
#else
  // XXX FIXME XXX
  return 24;
#endif
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
