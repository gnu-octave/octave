// defaults.h                                               -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if !defined (octave_defaults_h)
#define octave_defaults_h 1

#ifndef DEFAULT_PAGER
#define DEFAULT_PAGER "less -e"
#endif

#ifndef OCTAVE_PREFIX
#define OCTAVE_PREFIX "/usr/local"
#endif

#ifndef OCTAVE_EXEC_PREFIX
#define OCTAVE_EXEC_PREFIX "/usr/local"
#endif

#ifndef OCTAVE_DATADIR
#define OCTAVE_DATADIR "/usr/local/lib"
#endif

#ifndef OCTAVE_LIBDIR
#define OCTAVE_LIBDIR "/usr/local/lib"
#endif

#ifndef OCTAVE_BINDIR
#define OCTAVE_BINDIR "/usr/local/bin"
#endif

#ifndef OCTAVE_INFODIR
#define OCTAVE_INFODIR "/usr/local/info"
#endif

#ifndef OCTAVE_FCNFILEDIR
#define OCTAVE_FCNFILEDIR "/usr/local/lib/octave/ss-950127/m"
#endif

#ifndef OCTAVE_STARTUPFILEDIR
#define OCTAVE_STARTUPFILEDIR OCTAVE_FCNFILEDIR "/startup"
#endif

#ifndef OCTAVE_LOCALFCNFILEPATH
#define OCTAVE_LOCALFCNFILEPATH "/usr/local/lib/octave/site/m//"
#endif

#ifndef OCTAVE_ARCHLIBDIR
#define OCTAVE_ARCHLIBDIR "/usr/local/lib/octave/ss-950127/exec/sparc-sun-sunos4.1.2"
#endif

#ifndef OCTAVE_OCTFILEDIR
#define OCTAVE_OCTFILEDIR "/usr/local/lib/octave/ss-950127/oct/sparc-sun-sunos4.1.2"
#endif

#ifndef OCTAVE_LOCALOCTFILEPATH
#define OCTAVE_LOCALOCTFILEPATH "/usr/local/lib/octave/site/oct/sparc-sun-sunos4.1.2//"
#endif

#ifndef OCTAVE_FCNFILEPATH
#define OCTAVE_FCNFILEPATH ".:/usr/local/lib/octave/site/oct/sparc-sun-sunos4.1.2//:/usr/local/lib/octave/site/m//:/usr/local/lib/octave/ss-950127/oct/sparc-sun-sunos4.1.2//:/usr/local/lib/octave/ss-950127/m//"
#endif

#ifndef OCTAVE_IMAGEPATH
#define OCTAVE_IMAGEPATH ".:/usr/local/lib/octave/ss-950127/imagelib//"
#endif

#ifndef TARGET_HOST_TYPE
#define TARGET_HOST_TYPE "sparc-sun-sunos4.1.2"
#endif

#ifndef FLIB_LIST
#define FLIB_LIST "libF77.a"
#endif

#ifndef FLIB_PATH
#define FLIB_PATH "/usr/lang/SC1.0/cg87:/usr/lang/SC1.0"
#endif

#ifndef CXXLIB_LIST
#define CXXLIB_LIST "libg++.a:libg++.a:libgcc.a:libc.a:libgcc.a"
#endif

#ifndef CXXLIB_PATH
#define CXXLIB_PATH "/usr/local/gnu/lib/gcc-lib/sparc-sun-sunos4.1.2/2.6.3:/usr/local/gnu/lib"
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
