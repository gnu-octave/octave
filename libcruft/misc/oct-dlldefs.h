/*

Copyright (C) 2006, 2007, 2008 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_dlldefs_h)
#define octave_dlldefs_h 1

#if defined (_MSC_VER)
#define OCTAVE_EXPORT __declspec(dllexport)
#define OCTAVE_IMPORT __declspec(dllimport)
#else
/* All other compilers, at least for now. */
#define OCTAVE_EXPORT
#define OCTAVE_IMPORT
#endif

/* API macro for libcruft */
#ifdef CRUFT_DLL
#define CRUFT_API OCTAVE_EXPORT
#else
#define CRUFT_API OCTAVE_IMPORT
#endif

/* API macro for liboctave */
#ifdef OCTAVE_DLL
#define OCTAVE_API OCTAVE_EXPORT
#else
#define OCTAVE_API OCTAVE_IMPORT
#endif

/* API macro for src */
#ifdef OCTINTERP_DLL
#define OCTINTERP_API OCTAVE_EXPORT
#else
#define OCTINTERP_API OCTAVE_IMPORT
#endif

/* API macro for src/graphics */
#ifdef OCTGRAPHICS_DLL
#define OCTGRAPHICS_API OCTAVE_EXPORT
#else
#define OCTGRAPHICS_API OCTAVE_IMPORT
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
