/*

Copyright (C) 1996-2004 John W. Eaton

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

#define OCTAVE_VERSION "2.1.63"

#define OCTAVE_API_VERSION "api-v12"

#define OCTAVE_COPYRIGHT \
  "Copyright (C) 2004 John W. Eaton."

#define OCTAVE_NAME_AND_VERSION \
  "GNU Octave, version " OCTAVE_VERSION " (" OCTAVE_CANONICAL_HOST_TYPE ")"

#define OCTAVE_COPYING_STATEMENT \
  "This is free software; see the source code for copying conditions."

#define OCTAVE_WARRANTY_STATEMENT \
  "There is ABSOLUTELY NO WARRANTY; not even for MERCHANTIBILITY or\n\
FITNESS FOR A PARTICULAR PURPOSE."

#define OCTAVE_WWW_STATEMENT \
  "Additional information about Octave is available at http://www.octave.org."

#define OCTAVE_CONTRIB_STATEMENT \
  "Please contribute if you find this software useful.\n\
For more information, visit http://www.octave.org/help-wanted.html"

#define OCTAVE_BUGS_STATEMENT \
  "Report bugs to <bug@octave.org> (but first, please read\n\
http://www.octave.org/bugs.html to learn how to write a helpful report)."

#define OCTAVE_NAME_VERSION_AND_COPYRIGHT \
  OCTAVE_NAME_AND_VERSION ".\n" \
  OCTAVE_COPYRIGHT

#define OCTAVE_NAME_VERSION_COPYRIGHT_COPYING_AND_WARRANTY \
  OCTAVE_NAME_VERSION_AND_COPYRIGHT "\n" \
  OCTAVE_COPYING_STATEMENT "\n" \
  OCTAVE_WARRANTY_STATEMENT

#define X_OCTAVE_NAME_VERSION_COPYRIGHT_COPYING_WARRANTY_AND_BUGS(ARG) \
  OCTAVE_NAME_VERSION_COPYRIGHT_COPYING_AND_WARRANTY \
  ARG \
  OCTAVE_WWW_STATEMENT "\n\n" \
  OCTAVE_CONTRIB_STATEMENT "\n\n" \
  OCTAVE_BUGS_STATEMENT

#define OCTAVE_NAME_VERSION_COPYRIGHT_COPYING_WARRANTY_AND_BUGS \
  X_OCTAVE_NAME_VERSION_COPYRIGHT_COPYING_WARRANTY_AND_BUGS ("\n\n")

#define OCTAVE_STARTUP_MESSAGE \
  X_OCTAVE_NAME_VERSION_COPYRIGHT_COPYING_WARRANTY_AND_BUGS \
    ("  For details, type `warranty'.\n\n")

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
