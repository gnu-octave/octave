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

#include "DAE.h"
#include "lo-error.h"

DAE::DAE (const ColumnVector& x, const ColumnVector& xxdot,
	  double t, DAEFunc& f)
  : base_diff_eqn (x, t), DAEFunc (f), xdot (xxdot)
{
  if (x.length () != xdot.length ())
    ; // XXX FIXME XXX -- exception!
}

void
DAE::initialize (const ColumnVector& xx, double t)
{
  if (xx.length () != xdot.length ())
    ; // XXX FIXME XXX -- exception!
  else
    base_diff_eqn::initialize (xx, t);
}

void
DAE::initialize (const ColumnVector& xx, const ColumnVector& xxdot,
		 double t)
{
  if (xx.length () != xxdot.length ())
    ; // XXX FIXME XXX -- exception!
  else
    {
      base_diff_eqn::initialize (xx, t);
      xdot = xxdot;
    }
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
