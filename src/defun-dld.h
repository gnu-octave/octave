// defun-dld.h                                           -*- C++ -*-
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
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_defun_dld_h)
#define octave_defun_dld_h 1

#if defined (octave_defun_h)
#error defun.h and defun-dld.h both included in same file!
#endif

#include "defun-int.h"

// Define a builtin function that may be loaded dynamically at run
// time.
//
// If Octave is not configured for dynamic linking of builtin
// functions, this is exactly like DEFUN.

#if defined (OCTAVE_LITE) && defined (WITH_DYNAMIC_LINKING)
#if ! defined (MAKE_BUILTINS)
#define DEFUN_DLD_BUILTIN(name, fname, sname, fsname, unused_arg_flags, doc) \
  DEFUN_DLD(name, fname, sname, fsname, unused_arg_flags, doc)
#endif
#else
#define DEFUN_DLD_BUILTIN(name, fname, fsname, sname, unused_arg_flags, doc) \
  DEFUN_INTERNAL (name, fname, sname, unused_arg_flags, 0, doc)
#endif

// Define a function that may be loaded dynamically at run time.
//
// If Octave is not configured for dynamic linking of builtin
// functions, this won't do anything useful.
//
// The forward declaration is for the struct, the second is for the
// definition of the function.

#if ! defined (MAKE_BUILTINS)
#define DEFUN_DLD(name, fname, sname, fsname, unused_arg_flags, doc) \
  DECLARE_FUN_ ## unused_arg_flags(fname); \
  DEFINE_FUN_STRUCT (name, fname, sname, 0, doc); \
  DEFINE_FUN_STRUCT_FUN (sname, fsname) \
  DECLARE_FUN_ ## unused_arg_flags (fname)
#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
