/*

Copyright (C) 1995 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifndef HAVE_LGAMMA

#include "f77-uscore.h"

/* If the system doesn't have lgamma, assume that it doesn't have
   signgam either */

int signgam;

extern int F77_FCN (dlgams, DLGAMS) (const double&, double&, double&);

double
lgamma (double x)
{
  double result;
  double sgngam;

  F77_FCN (dlgams, DLGAMS) (x, result, sgngam);

  signgam = (int) sgngam;

  return result;
}

#endif

/*
;;; Local Variables: ***
;;; mode: C ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
