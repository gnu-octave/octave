/*

Copyright (C) 2002 John W. Eaton

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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

*/

#if !defined (octave_liboctave_sstream_h)
#define octave_liboctave_sstream_h 1

#if defined (__GNUG__) && __GNUC__ < 3
#undef HAVE_SSTREAM
#endif

#ifdef HAVE_SSTREAM

#include <sstream>

#define STRINGSTREAMBUF std::stringbuf

#define ISSTREAM std::istringstream

#define OSSTREAM std::ostringstream
#define OSSTREAM_STR(os) (os).str ()
// XXX FIXME XXX -- how long is the temporary created by the str()
// method guaranteed to exist?
#define OSSTREAM_C_STR(os) (os).str () . c_str ()
#define OSSTREAM_ENDS ""
#define OSSTREAM_FREEZE(os) do { } while (0)

#else

#include <strstream>

#define STRINGSTREAMBUF std::strstreambuf

#define ISSTREAM std::istrstream

#define OSSTREAM std::ostrstream
#define OSSTREAM_STR(os) std::string ((os).str ())
#define OSSTREAM_C_STR(os) (os).str ()
#define OSSTREAM_ENDS std::ends
#define OSSTREAM_FREEZE(os) do { (os).freeze (false); } while (0)

#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
