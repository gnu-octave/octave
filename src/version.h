/*

Copyright (C) 1996, 1997, 1998 John W. Eaton

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

#if !defined (octave_version_h)
#define octave_version_h 1

#define OCTAVE_VERSION "2.1.23"

#define OCTAVE_COPYRIGHT \
  "Copyright (C) 1996, 1997, 1998 John W. Eaton."

#define OCTAVE_NAME_AND_VERSION \
  "GNU Octave, version " OCTAVE_VERSION " (" CANONICAL_HOST_TYPE ")"

#define OCTAVE_NAME_VERSION_AND_COPYRIGHT \
  OCTAVE_NAME_AND_VERSION ".\n" OCTAVE_COPYRIGHT "\n\
This is free software with ABSOLUTELY NO WARRANTY."

#define OCTAVE_STARTUP_MESSAGE \
  OCTAVE_NAME_VERSION_AND_COPYRIGHT "\n\
For details, type `warranty'.\n\
\n\
*** This is a development version of Octave.  Development releases\n\
*** are provided for people who want to help test, debug, and improve\n\
*** Octave.\n\
***\n\
*** If you want a stable, well-tested version of Octave, you should be\n\
*** using one of the stable releases (when this development release\n\
*** was made, the latest stable version was 2.0.14)."

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
