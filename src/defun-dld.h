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
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.

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
// functions, this is the same as DEFUN, except that it will generate
// an extra externally visible function.
//
// The first DECLARE_FUN is for the benefit of the installer function
// and the second is for the definition of the function.

#if defined (MAKE_BUILTINS)

#define DEFUN_DLD(name, args_name, nargout_name, doc) \
  DEFUN_DLD_INTERNAL (name, args_name, nargout_name, false, doc)

// This one can be used when `name' cannot be used directly (if it is
// already defined as a macro).  In that case, name is already a
// quoted string, and the internal name of the function must be passed
// too (the convention is to use a prefix of "F", so "foo" becomes "Ffoo").

#define DEFUNX_DLD(name, fname, fsname, args_name, nargout_name, doc) \
  DEFUNX_DLD_INTERNAL (name, fname, args_name, nargout_name, false, doc)

#else

#define DEFUN_DLD(name, args_name, nargout_name, doc) \
  DECLARE_FUN (name, args_name, nargout_name); \
  DEFINE_FUN_INSTALLER_FUN (name, doc) \
  DECLARE_FUN (name, args_name, nargout_name)

#define DEFUNX_DLD(name, fname, fsname, args_name, nargout_name, doc) \
  DECLARE_FUNX (fname, args_name, nargout_name); \
  DEFINE_FUNX_INSTALLER_FUN (name, fname, fsname, doc) \
  DECLARE_FUNX (fname, args_name, nargout_name)

#endif

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
