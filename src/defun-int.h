// defun-int.h                                          -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994 John W. Eaton

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

#if !defined (octave_defun_int_h)
#define octave_defun_int_h 1

#ifdef MAKE_BUILTINS

#define DEFUN_INTERNAL(name, fname, sname, nargin_max, nargout_max, \
		       is_text_fcn, doc) \
  BEGIN_INSTALL_BUILTIN \
    extern DECLARE_FUN(fname); \
    static builtin_function sname = \
      { name, nargin_max, nargout_max, is_text_fcn, fname, doc }; \
    install_builtin_function (&sname); \
  END_INSTALL_BUILTIN

#define DEFALIAS_INTERNAL(alias, name)

#else /* ! MAKE_BUILTINS */

#define DEFUN_INTERNAL(name, fname, sname, nargin_max, nargout_max, \
		       is_text_fcn, doc) \
  DECLARE_FUN(fname)

#define DEFALIAS_INTERNAL(alias, name)

#endif /* ! MAKE_BUILTINS */

#define DECLARE_FUN(fname) \
  Octave_object \
  fname (const Octave_object& args, int nargout)

// XXX FIXME XXX -- eliminate the need for these in the functions that
// use them?

#define DEFINE_ARGV(warnfor) \
  int argc = args.length (); \
  int save_argc = argc; \
  char **argv = make_argv (args, warnfor); \
  char **save_argv = argv; \
  if (error_state) \
    return retval

#define DELETE_ARGV \
  do \
    { \
      while (--save_argc >= 0) \
	delete [] save_argv[save_argc]; \
      delete [] save_argv; \
    } \
  while (0)

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
