// terminals.cc                                              -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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

#ifdef __GNUG__
#pragma implementation
#endif

#include <stddef.h>
#include <strings.h>
#include "terminals.h"

/*
 * It would be nice to be able to get these directly from gnuplot
 * during the configuration/build procedure.
 */
static char *valid_terminals[] = 
{
  "unknown",
  "table",
  "dumb",
  "aed512",
  "aed767",
  "bitgraph",
  "dxy800a",
  "eepic",
  "emtex",
  "epson_60dpi",
  "epson_lx800",
  "fig",
  "bfig",
  "hp2623A",
  "hp2648",
  "hp7580B",
  "hpgl",
  "hpljii",
  "hpdj",
  "pcl5_port",
  "pcl5_land",
  "imagen",
  "kc_tek40",
  "km_tek40",
  "latex",
  "nec_cp6m",
  "nec_cp6c",
  "nec_cp6d",
  "pbm",
  "pgm",
  "ppm",
  "postscript",
  "prescribe",
  "kyo",
  "qms",
  "regis",
  "selanar",
  "starc",
  "tandy_60dpi",
  "tek410",
  "tek40",
  "unixplot",
  "vx384",
  "vttek",
  "x11",
  "X11",
  (char *) NULL,
};

/*
 * Is the given terminal named in the list above?
 */
int
valid_terminal (char *term)
{
  if (term == (char *) NULL)
    return 0;

  for (char **t_list = valid_terminals; *t_list != (char *) NULL; t_list++)
    {
      char *t = *t_list;
      int len = strlen (t);
      if (strncmp (term, t, len) == 0)
	return 1;
    }
  return 0;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
